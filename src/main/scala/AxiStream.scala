//vim: tabstop=4 expandtab shiftwidth=4 softtabstop=4
package AXIStream {

  import chisel3._

  class sAxiStreamIF(val busWidth:Int, val tuserWidth:Int) extends Bundle {
    val tdata = Input(UInt(busWidth.W))
    val tuser = Input(UInt(tuserWidth.W))
    val tvalid = Input(UInt(1.W))
    val tready = Output(UInt(1.W))
    val tlast = Input(UInt(1.W))
  }

  class mAxiStreamIF(val busWidth:Int, val tuserWidth:Int) extends Bundle {
    val tdata = Output(UInt(busWidth.W))
    val tuser = Output(UInt(tuserWidth.W))
    val tvalid = Output(UInt(1.W))
    val tready = Input(UInt(1.W))
    val tlast = Output(UInt(1.W))
  }

}// end of package
