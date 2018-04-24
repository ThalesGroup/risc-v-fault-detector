// vim: tabstop=2 expandtab shiftwidth=2 softtabstop=2
package deinterlacer
import chisel3._

class Memory(val depth:Int, val wordWidth:Int) extends Module {
  val io = IO(new Bundle{
    val wea = Input(Bool())
    val ena = Input(Bool())
    val enb = Input(Bool())
    val waddr = Input(UInt(12.W))
    val raddr = Input(UInt(12.W))
    val dataIn = Input(UInt(wordWidth.W))
    val dataOut = Output(UInt(wordWidth.W))
  })

  val mem = Mem(depth, UInt(wordWidth.W))
  val data_out = RegInit(0.U(wordWidth.W))

  io.dataOut := data_out

  when(io.ena) {
    when(io.wea) {
      mem.write(io.waddr, io.dataIn)
    }
  }

  when(io.enb) {
    data_out := mem.read(io.raddr)
  }

}
