// vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
package fault_detector
import chisel3._
import chisel3.experimental.withReset
import chisel3.util._
import freechips.rocketchip.amba.axi4._

case class cpuRepair() extends Module{
  val io = IO(new Bundle{
    val fault = Input(Bool())
    val cpus_ok = Input(Bool())
    val reset_pending = Input(Bool())
    val ack_cpus_ok = Output(Bool())
    val stop_cpus = Output(Bool())
    val latch_regs = Output(Bool())
    val reset_to_recovery = Output(Bool())
    val reset_cpus = Output(Bool())
    val reset_routing_logic = Output(Bool())
    val cpu_in_interrupt = Input(Bool())
  })

  val sIdle :: sFaultDetected :: sStopCPUs :: sLatchRegs :: sResetToRecovery :: sWaitForReset :: sWaitForResetEnd :: sWaitForCPU :: Nil = Enum(8)
  val state = RegInit(sIdle)

  val ack_cpus_ok = RegInit(false.B)
  val stop_cpus = RegInit(false.B)
  val latch_regs = RegInit(false.B)
  val reset_to_recovery = RegInit(false.B)
  val reset_cpus = RegInit(false.B)
  val reset_routing_logic = RegInit(false.B)

  switch (state) {
    is (sIdle) {
      stop_cpus := false.B
      ack_cpus_ok := false.B
      reset_to_recovery := false.B
      reset_routing_logic := false.B
      when(io.fault){
        state := sFaultDetected
      }
    }
    is (sFaultDetected) {
      stop_cpus := true.B
      state := sStopCPUs
      reset_to_recovery := true.B
    }
    is (sStopCPUs) {
      when(!io.cpu_in_interrupt) {
        state := sLatchRegs
        latch_regs := true.B
      }
    }
    is (sLatchRegs) {
      state := sResetToRecovery
      latch_regs := false.B
    }
    is (sResetToRecovery) {
      state := sWaitForReset
      reset_cpus := true.B
      stop_cpus := false.B
    }
    is (sWaitForReset) {
      when(io.reset_pending) {
        state := sWaitForResetEnd
      }
    }
    is (sWaitForResetEnd) {
      when(!io.reset_pending) {
        state := sWaitForCPU
        //reset_to_recovery := false.B
        reset_routing_logic := true.B
        reset_cpus := false.B
      }
    }
    is (sWaitForCPU) {
      reset_cpus := false.B
      when(io.cpus_ok) {
        reset_routing_logic := false.B
        ack_cpus_ok := true.B
        state := sIdle
      }
    }
  }

  io.ack_cpus_ok := ack_cpus_ok
  io.stop_cpus := stop_cpus
  io.latch_regs := latch_regs
  io.reset_to_recovery := reset_to_recovery
  io.reset_cpus := reset_cpus
  io.reset_routing_logic := reset_routing_logic

}


case class signalHolder (length: Int) extends Module{
  val io = IO(new Bundle{
    val rst = Input(Bool())
    val o = Output(UInt(1.W))
    val i = Input(UInt(1.W))
  })

  withReset(io.rst) {
    val count = Counter(length)
    val stop = RegInit(0.U(1.W))

    when(io.i.toBool) {
      when(count.inc()) {
        stop := 1.U
      }
    }
    io.o := io.i & ~(stop)
  }

}

case class AXIForwarder(input: AXI4Bundle, output: AXI4Bundle, stop: Bool, AXIParameters: AXI4BundleParameters) {
  val axiBus = Reg(new AXI4Bundle(AXIParameters))
  val axiBus_d = Reg(new AXI4Bundle(AXIParameters))

  def connect(invalid: Bool) {
  when(!stop) {
        axiBus <> input
        axiBus_d <> axiBus
        output <> axiBus_d
      when(invalid) {
        input.aw.ready := 0.U
        input.w.ready := 0.U
        input.ar.ready := 0.U

        input.b.valid := 0.U
        input.b.bits.id := 0.U
        input.b.bits.resp := 0.U

        input.r.valid := 0.U
        input.r.bits.id := 0.U
        input.r.bits.resp := 0.U

        input.r.bits.last := 0.U
        input.r.bits.data := 0.U
      }
    }  .otherwise {
      axiBus <> input
      output <> axiBus_d
    }
  }
}

case class AXIMerger(inputs : Seq[AXI4Bundle], registered_inputs : Seq[AXI4Bundle], output : AXI4Bundle) {

  def merge(invalid_pairs: UInt) {

    // outputs on AXI slave IF
    registered_inputs.foreach { axi =>
      axi.aw.ready := output.aw.ready
      axi.w.ready := output.w.ready
      axi.ar.ready := output.ar.ready

      axi.b.valid := output.b.valid
      axi.b.bits.id := output.b.bits.id
      axi.b.bits.resp := output.b.bits.resp
      //axi.b.bits.user := output.b.bits.user

      axi.r.valid := output.r.valid
      axi.r.bits.id := output.r.bits.id
      axi.r.bits.resp := output.r.bits.resp
      //axi.r.bits.user := output.r.bits.user
      axi.r.bits.last := output.r.bits.last
      axi.r.bits.data := output.r.bits.data
    }
    /* possible conbinations
     * 0x6 - CPU0 is faulty
     * 0x5 - CPU1 is faulty
     * 0x3 - CPU2 is faulty
     * 0x7 - all of them gave different results
     */

    when(invalid_pairs === 0x6.U){

      output.aw.valid := registered_inputs(1).aw.valid | registered_inputs(2).aw.valid
      output.w.valid  := registered_inputs(1).w.valid  | registered_inputs(2).w.valid
      output.ar.valid := registered_inputs(1).ar.valid | registered_inputs(2).ar.valid

      output.b.ready := registered_inputs(1).b.ready | registered_inputs(2).b.ready
      output.r.ready := registered_inputs(1).r.ready | registered_inputs(2).r.ready

      output.aw.bits.id    := registered_inputs(1).aw.bits.id    | registered_inputs(2).aw.bits.id
      output.aw.bits.addr  := registered_inputs(1).aw.bits.addr  | registered_inputs(2).aw.bits.addr
      output.aw.bits.len   := registered_inputs(1).aw.bits.len   | registered_inputs(2).aw.bits.len
      output.aw.bits.size  := registered_inputs(1).aw.bits.size  | registered_inputs(2).aw.bits.size
      output.aw.bits.burst := registered_inputs(1).aw.bits.burst | registered_inputs(2).aw.bits.burst
      output.aw.bits.lock  := registered_inputs(1).aw.bits.lock  | registered_inputs(2).aw.bits.lock
      output.aw.bits.cache := registered_inputs(1).aw.bits.cache | registered_inputs(2).aw.bits.cache
      output.aw.bits.prot  := registered_inputs(1).aw.bits.prot  | registered_inputs(2).aw.bits.prot
      output.aw.bits.qos   := registered_inputs(1).aw.bits.qos   | registered_inputs(2).aw.bits.qos

      output.ar.bits.id    := registered_inputs(1).ar.bits.id    | registered_inputs(2).ar.bits.id
      output.ar.bits.addr  := registered_inputs(1).ar.bits.addr  | registered_inputs(2).ar.bits.addr
      output.ar.bits.len   := registered_inputs(1).ar.bits.len   | registered_inputs(2).ar.bits.len
      output.ar.bits.size  := registered_inputs(1).ar.bits.size  | registered_inputs(2).ar.bits.size
      output.ar.bits.burst := registered_inputs(1).ar.bits.burst | registered_inputs(2).ar.bits.burst
      output.ar.bits.lock  := registered_inputs(1).ar.bits.lock  | registered_inputs(2).ar.bits.lock
      output.ar.bits.cache := registered_inputs(1).ar.bits.cache | registered_inputs(2).ar.bits.cache
      output.ar.bits.prot  := registered_inputs(1).ar.bits.prot  | registered_inputs(2).ar.bits.prot
      output.ar.bits.qos   := registered_inputs(1).ar.bits.qos   | registered_inputs(2).ar.bits.qos

      output.w.bits.data := registered_inputs(1).w.bits.data | registered_inputs(2).w.bits.data
      output.w.bits.strb := registered_inputs(1).w.bits.strb | registered_inputs(2).w.bits.strb
      output.w.bits.last := registered_inputs(1).w.bits.last | registered_inputs(2).w.bits.last

    }.elsewhen(invalid_pairs === 0x5.U){

      output.aw.valid := registered_inputs(0).aw.valid | registered_inputs(2).aw.valid
      output.w.valid  := registered_inputs(0).w.valid  | registered_inputs(2).w.valid
      output.ar.valid := registered_inputs(0).ar.valid | registered_inputs(2).ar.valid

      output.b.ready := registered_inputs(0).b.ready | registered_inputs(2).b.ready
      output.r.ready := registered_inputs(0).r.ready | registered_inputs(2).r.ready

      output.aw.bits.id    := registered_inputs(0).aw.bits.id    | registered_inputs(2).aw.bits.id
      output.aw.bits.addr  := registered_inputs(0).aw.bits.addr  | registered_inputs(2).aw.bits.addr
      output.aw.bits.len   := registered_inputs(0).aw.bits.len   | registered_inputs(2).aw.bits.len
      output.aw.bits.size  := registered_inputs(0).aw.bits.size  | registered_inputs(2).aw.bits.size
      output.aw.bits.burst := registered_inputs(0).aw.bits.burst | registered_inputs(2).aw.bits.burst
      output.aw.bits.lock  := registered_inputs(0).aw.bits.lock  | registered_inputs(2).aw.bits.lock
      output.aw.bits.cache := registered_inputs(0).aw.bits.cache | registered_inputs(2).aw.bits.cache
      output.aw.bits.prot  := registered_inputs(0).aw.bits.prot  | registered_inputs(2).aw.bits.prot
      output.aw.bits.qos   := registered_inputs(0).aw.bits.qos   | registered_inputs(2).aw.bits.qos

      output.ar.bits.id    := registered_inputs(0).ar.bits.id    | registered_inputs(2).ar.bits.id
      output.ar.bits.addr  := registered_inputs(0).ar.bits.addr  | registered_inputs(2).ar.bits.addr
      output.ar.bits.len   := registered_inputs(0).ar.bits.len   | registered_inputs(2).ar.bits.len
      output.ar.bits.size  := registered_inputs(0).ar.bits.size  | registered_inputs(2).ar.bits.size
      output.ar.bits.burst := registered_inputs(0).ar.bits.burst | registered_inputs(2).ar.bits.burst
      output.ar.bits.lock  := registered_inputs(0).ar.bits.lock  | registered_inputs(2).ar.bits.lock
      output.ar.bits.cache := registered_inputs(0).ar.bits.cache | registered_inputs(2).ar.bits.cache
      output.ar.bits.prot  := registered_inputs(0).ar.bits.prot  | registered_inputs(2).ar.bits.prot
      output.ar.bits.qos   := registered_inputs(0).ar.bits.qos   | registered_inputs(2).ar.bits.qos

      output.w.bits.data := registered_inputs(0).w.bits.data | registered_inputs(2).w.bits.data
      output.w.bits.strb := registered_inputs(0).w.bits.strb | registered_inputs(2).w.bits.strb
      output.w.bits.last := registered_inputs(0).w.bits.last | registered_inputs(2).w.bits.last

    }.elsewhen(invalid_pairs === 0x3.U){

      output.aw.valid := registered_inputs(0).aw.valid | registered_inputs(1).aw.valid
      output.w.valid  := registered_inputs(0).w.valid  | registered_inputs(1).w.valid
      output.ar.valid := registered_inputs(0).ar.valid | registered_inputs(1).ar.valid

      output.b.ready := registered_inputs(0).b.ready | registered_inputs(1).b.ready
      output.r.ready := registered_inputs(0).r.ready | registered_inputs(1).r.ready

      output.aw.bits.id    := registered_inputs(0).aw.bits.id    | registered_inputs(1).aw.bits.id
      output.aw.bits.addr  := registered_inputs(0).aw.bits.addr  | registered_inputs(1).aw.bits.addr
      output.aw.bits.len   := registered_inputs(0).aw.bits.len   | registered_inputs(1).aw.bits.len
      output.aw.bits.size  := registered_inputs(0).aw.bits.size  | registered_inputs(1).aw.bits.size
      output.aw.bits.burst := registered_inputs(0).aw.bits.burst | registered_inputs(1).aw.bits.burst
      output.aw.bits.lock  := registered_inputs(0).aw.bits.lock  | registered_inputs(1).aw.bits.lock
      output.aw.bits.cache := registered_inputs(0).aw.bits.cache | registered_inputs(1).aw.bits.cache
      output.aw.bits.prot  := registered_inputs(0).aw.bits.prot  | registered_inputs(1).aw.bits.prot
      output.aw.bits.qos   := registered_inputs(0).aw.bits.qos   | registered_inputs(1).aw.bits.qos

      output.ar.bits.id    := registered_inputs(0).ar.bits.id    | registered_inputs(1).ar.bits.id
      output.ar.bits.addr  := registered_inputs(0).ar.bits.addr  | registered_inputs(1).ar.bits.addr
      output.ar.bits.len   := registered_inputs(0).ar.bits.len   | registered_inputs(1).ar.bits.len
      output.ar.bits.size  := registered_inputs(0).ar.bits.size  | registered_inputs(1).ar.bits.size
      output.ar.bits.burst := registered_inputs(0).ar.bits.burst | registered_inputs(1).ar.bits.burst
      output.ar.bits.lock  := registered_inputs(0).ar.bits.lock  | registered_inputs(1).ar.bits.lock
      output.ar.bits.cache := registered_inputs(0).ar.bits.cache | registered_inputs(1).ar.bits.cache
      output.ar.bits.prot  := registered_inputs(0).ar.bits.prot  | registered_inputs(1).ar.bits.prot
      output.ar.bits.qos   := registered_inputs(0).ar.bits.qos   | registered_inputs(1).ar.bits.qos

      output.w.bits.data := registered_inputs(0).w.bits.data | registered_inputs(1).w.bits.data
      output.w.bits.strb := registered_inputs(0).w.bits.strb | registered_inputs(1).w.bits.strb
      output.w.bits.last := registered_inputs(0).w.bits.last | registered_inputs(1).w.bits.last

    }.otherwise{

      // outputs on AXI master IF
      output.aw.valid := registered_inputs.aggregate(false.B)({ (res, v) => res|v.aw.valid}, {(p1, p2) => p1|p2})
      output.w.valid  := registered_inputs.aggregate(false.B)({ (res, v) => res|v.w.valid}, {(p1, p2) => p1|p2})
      output.ar.valid := registered_inputs.aggregate(false.B)({ (res, v) => res|v.ar.valid}, {(p1, p2) => p1|p2})

      output.b.ready := registered_inputs.aggregate(false.B)({ (res, v) => res|v.b.ready}, {(p1, p2) => p1|p2})
      output.r.ready := registered_inputs.aggregate(false.B)({ (res, v) => res|v.r.ready}, {(p1, p2) => p1|p2})

      output.aw.bits.id    := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.id}, {(p1, p2) => p1|p2} )
      output.aw.bits.addr  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.addr}, {(p1, p2) => p1|p2} )
      output.aw.bits.len   := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.len}, {(p1, p2) => p1|p2} )
      output.aw.bits.size  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.size}, {(p1, p2) => p1|p2} )
      output.aw.bits.burst := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.burst}, {(p1, p2) => p1|p2} )
      output.aw.bits.lock  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.lock}, {(p1, p2) => p1|p2} )
      output.aw.bits.cache := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.cache}, {(p1, p2) => p1|p2} )
      output.aw.bits.prot  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.prot}, {(p1, p2) => p1|p2} )
      output.aw.bits.qos   := registered_inputs.aggregate(0.U)({ (res, v) => res|v.aw.bits.qos}, {(p1, p2) => p1|p2} )

      output.ar.bits.id    := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.id}, {(p1, p2) => p1|p2} )
      output.ar.bits.addr  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.addr}, {(p1, p2) => p1|p2} )
      output.ar.bits.len   := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.len}, {(p1, p2) => p1|p2} )
      output.ar.bits.size  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.size}, {(p1, p2) => p1|p2} )
      output.ar.bits.burst := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.burst}, {(p1, p2) => p1|p2} )
      output.ar.bits.lock  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.lock}, {(p1, p2) => p1|p2} )
      output.ar.bits.cache := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.cache}, {(p1, p2) => p1|p2} )
      output.ar.bits.prot  := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.prot}, {(p1, p2) => p1|p2} )
      output.ar.bits.qos   := registered_inputs.aggregate(0.U)({ (res, v) => res|v.ar.bits.qos}, {(p1, p2) => p1|p2} )

      output.w.bits.data := registered_inputs.aggregate(0.U)({ (res, v) => res|v.w.bits.data}, {(p1, p2) => p1|p2} )
      output.w.bits.strb := registered_inputs.aggregate(0.U)({ (res, v) => res|v.w.bits.strb}, {(p1, p2) => p1|p2} )
      output.w.bits.last := registered_inputs.aggregate(0.U)({ (res, v) => res|v.w.bits.last}, {(p1, p2) => p1|p2} )
    }

  }

  // monitor all input signals
  def monitor_buses(stop: Bool): UInt = {

    val pairs_nok = Wire(Vec(inputs.combinations(2).length, Bool()))
    // each AXI slave IF has 26 inputs
    val inputs_status = Wire(Vec(inputs.combinations(2).length, Vec(26, UInt(1.W))))

    inputs.combinations(2).zipWithIndex.foreach { case(bus, pair) =>

        val pair_status = inputs_status(pair)
        pair_status(0) := bus(0).aw.valid =/= bus(1).aw.valid
        pair_status(1) := bus(0).w.valid =/=bus(1).w.valid
        pair_status(2) := bus(0).ar.valid =/= bus(1).ar.valid

        pair_status(3) := bus(0).b.ready =/= bus(1).b.ready
        pair_status(4) := bus(0).r.ready =/= bus(1).r.ready

        pair_status(5) := (bus(0).aw.bits.id ^ bus(1).aw.bits.id).orR
        pair_status(6) := (bus(0).aw.bits.addr ^ bus(1).aw.bits.addr).orR
        pair_status(7) := (bus(0).aw.bits.len ^ bus(1).aw.bits.len).orR
        pair_status(8) := (bus(0).aw.bits.size ^ bus(1).aw.bits.size).orR
        pair_status(9) := (bus(0).aw.bits.burst ^ bus(1).aw.bits.burst).orR
        pair_status(10) := (bus(0).aw.bits.lock ^ bus(1).aw.bits.lock).orR
        pair_status(11) := (bus(0).aw.bits.cache ^ bus(1).aw.bits.cache).orR
        pair_status(12) := (bus(0).aw.bits.prot ^ bus(1).aw.bits.prot).orR
        pair_status(13) := (bus(0).aw.bits.qos ^ bus(1).aw.bits.qos).orR

        pair_status(14) := (bus(0).ar.bits.id ^ bus(1).ar.bits.id).orR
        pair_status(15) := (bus(0).ar.bits.addr ^ bus(1).ar.bits.addr).orR
        pair_status(16) := (bus(0).ar.bits.len ^ bus(1).ar.bits.len).orR
        pair_status(17) := (bus(0).ar.bits.size ^ bus(1).ar.bits.size).orR
        pair_status(18) := (bus(0).ar.bits.burst ^ bus(1).ar.bits.burst).orR
        pair_status(19) := (bus(0).ar.bits.lock ^ bus(1).ar.bits.lock).orR
        pair_status(20) := (bus(0).ar.bits.cache ^ bus(1).ar.bits.cache).orR
        pair_status(21) := (bus(0).ar.bits.prot ^ bus(1).ar.bits.prot).orR
        pair_status(22) := (bus(0).ar.bits.qos ^ bus(1).ar.bits.qos).orR

        pair_status(23) := (bus(0).w.bits.data ^ bus(1).w.bits.data).orR
        pair_status(24) := (bus(0).w.bits.strb ^ bus(1).w.bits.strb).orR
        pair_status(25) := (bus(0).w.bits.last ^ bus(1).w.bits.last).orR

        pairs_nok(pair) := pair_status.exists(_ === 1.U)
    }

    // merge differences into one fail signal
    Mux(stop, 0.U, pairs_nok.aggregate(0.U)({ (res, p) => Cat(res, p)}, {(p1,p2) => Cat(p1, p2)}))

  }

}

class FaultDetector(val busWidth:Int) extends Module {

  val AXIBusParameters = AXI4BundleParameters(32, 32, 4, 0)
  val reset_len = 100

  val io = IO(new Bundle{
    val invalid = Output(UInt(3.W))
    val cpu0_axi4 = Flipped(new AXI4Bundle(AXIBusParameters))
    val cpu1_axi4 = Flipped(new AXI4Bundle(AXIBusParameters))
    val cpu2_axi4 = Flipped(new AXI4Bundle(AXIBusParameters))

    val out_axi4 = new AXI4Bundle(AXIBusParameters)

    val fault_reset_vector = Output(UInt(3.W))
    val reset_cpu = Output(UInt(3.W))
    val stop_all_cpus = Output(Bool())
    val latch_registers = Output(Bool())
    val reset_to_recovery = Output(Bool())

    val cpu_reset_feedback = Input(Bool())

    val disable_reset = Input(UInt(1.W))

    val reset_routing_logic = Input(UInt(3.W))

    val cpu_back_online = Output(UInt(1.W))
    val ack_back_online = Input(UInt(1.W))
    val cpu_in_interrupt = Input(UInt(1.W))
  })

  val cpu_repair = Module(new cpuRepair)

  val axiBusInternal0 = Wire(new AXI4Bundle(AXIBusParameters))
  val axiBusInternal1 = Wire(new AXI4Bundle(AXIBusParameters))
  val axiBusInternal2 = Wire(new AXI4Bundle(AXIBusParameters))

  val inputs = Seq[AXI4Bundle](io.cpu0_axi4, io.cpu1_axi4, io.cpu2_axi4)
  val registered_inputs = Seq[AXI4Bundle](axiBusInternal0, axiBusInternal1, axiBusInternal2)

  /* keep reset routing logic high for 10 clk cycles */
  val reset_logic = Wire(Bool()) //io.reset_routing_logic.or
  val reset_routing = Vec(Reg(UInt(3.W)).toBools)
  val logic_reset_hold = Seq[signalHolder](Module(new signalHolder(reset_len)), Module(new signalHolder(reset_len)), Module(new signalHolder(reset_len)))

  (reset_routing zip logic_reset_hold).foreach { case (ri, hold) =>
    hold.io.rst := reset_logic
    hold.io.i := ~(reset_logic)
    ri := hold.io.o
  }

  val forwarder0 = new AXIForwarder(io.cpu0_axi4, axiBusInternal0, reset_routing(0), AXIBusParameters)
  val forwarder1 = new AXIForwarder(io.cpu1_axi4, axiBusInternal1, reset_routing(1), AXIBusParameters)
  val forwarder2 = new AXIForwarder(io.cpu2_axi4, axiBusInternal2, reset_routing(2), AXIBusParameters)

  val merger = new AXIMerger(inputs, registered_inputs, io.out_axi4)

  val cpu_reset_hold = Seq[signalHolder](Module(new signalHolder(reset_len)), Module(new signalHolder(reset_len)), Module(new signalHolder(reset_len)))
  val cpu_reset_im = Reg(UInt(3.W))

  val cpu_reset_out = Vec(Reg(UInt(3.W)).toBools)

  val invalid = merger.monitor_buses(reset_routing.asUInt.orR)
  val invalid_reg = RegInit(0.U(3.W))
  val faulty_cpu = RegInit(0.U(3.W))

  val reset_logic_reg = RegInit(false.B)

  val cpu_back_online_reg = RegInit(0.U(1.W))
  val ack_back_online = Wire(Bool())

  when(reset_logic.toBool) {
    reset_logic_reg := true.B
    cpu_reset_im := 0.U
    cpu_reset_out.foreach { cpu => cpu := false.B}
    invalid_reg := 0.U
    faulty_cpu := 0.U
  }

  // wait until the buses are OK
  when(reset_logic_reg) {
    when(invalid === 0.U) {
      reset_logic_reg := false.B
      cpu_back_online_reg := 1.U
    }
  }

  when(ack_back_online) {
    cpu_back_online_reg := 0.U
  }

  io.cpu_back_online := cpu_back_online_reg

  when(invalid =/= 0.U && !reset_logic_reg.toBool) {
    invalid_reg := invalid
  }

  when(invalid_reg =/= 0.U) {
    //io.stop_cpu := ~(invalid_reg)
    cpu_reset_im := ~(invalid_reg)
    faulty_cpu := ~(invalid_reg)
  }.otherwise {
    //io.stop_cpu := 0.U
    cpu_reset_im := 0.U
    faulty_cpu := 0.U
  }

  /* connect reset logic */
  cpu_reset_hold.zipWithIndex.foreach { case(hold, i) =>
    hold.io.rst := reset_logic
    hold.io.i := cpu_reset_im(i)
    cpu_reset_out(i) := hold.io.o
  }

  val invalid_bool = faulty_cpu.toBools

  forwarder0.connect(invalid_bool(0))
  forwarder1.connect(invalid_bool(1))
  forwarder2.connect(invalid_bool(2))

  merger.merge(invalid_reg)

  val reset_all_cpus = Wire(Bool())
  val reset_all_cpus_out = Wire(Bool())
  val reset_all_cpus_out_hold = Module(new signalHolder(reset_len))

  reset_all_cpus_out_hold.io.rst := reset_logic
  reset_all_cpus_out_hold.io.i := reset_all_cpus
  reset_all_cpus_out := reset_all_cpus_out_hold.io.o

  io.reset_cpu := Mux(reset_all_cpus_out, 0x7.U, Mux(reset_logic.toBool, 0.U, Mux(io.disable_reset.toBool, 0.U, cpu_reset_out.asUInt)))
  io.fault_reset_vector := Mux(reset_logic.toBool, 0.U, Mux(invalid_reg.orR.toBool, ~(invalid_reg), 0.U))
  io.invalid := Mux(reset_logic.toBool, 0.U, invalid_reg)

  /* connect cpuRepair logic */

  cpu_repair.io.fault := invalid_reg.orR.toBool & ~(cpu_reset_out.asUInt.orR.toBool)
  cpu_repair.io.cpus_ok := cpu_back_online_reg.toBool
  cpu_repair.io.reset_pending := reset_all_cpus_out | io.cpu_reset_feedback
  io.stop_all_cpus := cpu_repair.io.stop_cpus
  io.latch_registers := cpu_repair.io.latch_regs
  io.reset_to_recovery := cpu_repair.io.reset_to_recovery
  reset_all_cpus := cpu_repair.io.reset_cpus
  reset_logic := cpu_repair.io.reset_routing_logic
  ack_back_online := cpu_repair.io.ack_cpus_ok
  cpu_repair.io.cpu_in_interrupt := io.cpu_in_interrupt
}

object FaultDetector extends App {
  if(args.length == 0) {
    chisel3.Driver.execute(args, () => new FaultDetector(32))
  } else {
    iotesters.Driver.execute(args, () => new FaultDetector(32)){ c => new FaultDetectorTests(c) }
  }
}
