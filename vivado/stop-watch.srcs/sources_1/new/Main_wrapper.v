`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 03.06.2016 15:19:16
// Design Name: 
// Module Name: Main_wrapper
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////


module Main_wrapper(
        input         clk,
        input         btnL,
        input         btnC,
        input         btnR,
        output [6:0]  seg,
        output        dp,
        output [3:0]  an
    );

wire        segs_clk;
reg  [18:0] counter;

always@(posedge clk) begin
    if (counter < 399999)
        counter = counter + 1;
    else
        counter = 0;
end

assign segs_clk = counter == 0;

Main_topEntity topEntity (
        .input_0_0(btnR),
        .input_0_1(btnC),
        .input_0_2(btnL),
        .system1000(segs_clk),
        .system1000_rstn(1'b1),
        .output_0_0({seg, dp}),
        .output_0_1(an)
    );


endmodule
