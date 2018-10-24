/*  
*   Copyright © 2018. Thales SA 
*   All rights reserved
*   
*   Redistribution and use in source and binary forms, with or without
*   modification, are permitted provided that the following conditions
*   are met:
*   
*   1. Redistributions of source code must retain the above copyright
*      notice, this list of conditions and the following disclaimer.
*   
*   2. Redistributions in binary form must reproduce the above copyright
*      notice, this list of conditions and the following disclaimer in the
*      documentation and/or other materials provided with the distribution.
*   
*   3. Neither the name of the copyright holders nor the names of its
*      contributors may be used to endorse or promote products derived from
*      this software without specific prior written permission.
*   
*   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
*   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
*   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
*   ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
*   LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
*   CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
*   SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
*   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
*   CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
*   ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
*   THE POSSIBILITY OF SUCH DAMAGE.
*/

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
