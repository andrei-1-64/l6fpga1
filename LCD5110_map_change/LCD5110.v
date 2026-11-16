
module LCD5110 (
    input wire clk, S1, S2,
    output reg lcd_rst = 0, lcd_ce = 1,
    output reg lcd_dc = 0, lcd_din = 0, lcd_clk = 0
);

    reg [7:0] img1 [0:503], img2 [0:503];

    initial begin
        $readmemh("GAGNIUC_84x48.hex", img1);
        $readmemh("imagine2.hex", img2);
    end

    function [7:0] cmd;
        input [2:0] n;
        begin
            case (n)
                0: cmd=8'h21; 1: cmd=8'hB8;
                2: cmd=8'h04; 3: cmd=8'h14;
                4: cmd=8'h20; 5: cmd=8'h0C;
                6: cmd=8'h80; 7: cmd=8'h40;
            endcase
        end
    endfunction

    // Butoane active LOW
    reg [1:0] b1 = 2'b11, b2 = 2'b11;

    always @(posedge clk) begin
        b1 <= {b1[0], S1};
        b2 <= {b2[0], S2};
    end

    reg selected = 0, done = 0;
    reg [21:0] startup = 0;
    reg [6:0] div = 0;
    reg [9:0] index = 0;
    reg [7:0] shift = 0;
    reg [2:0] bit_no = 7;
    reg [1:0] state = 0;

    wire [9:0] pixel_index = index - 10'd8;

    wire [7:0] data_byte = (index < 8) ?
        cmd(index[2:0]) :
        selected ? img2[pixel_index] : img1[pixel_index];

    always @(posedge clk) begin

        if (startup < 22'd2500000) begin
            startup <= startup + 1'b1;
            if (startup == 22'd1000000)
                lcd_rst <= 1'b1;
        end

        // Schimbarea imaginii
        else if (done) begin
            if (!b1[1] && b2[1] && selected) begin
                selected <= 0;
                index <= 6;
                done <= 0;
            end
            else if (!b2[1] && b1[1] && !selected) begin
                selected <= 1;
                index <= 6;
                done <= 0;
            end
        end

        else if (div == 7'd99) begin
            div <= 0;

            case (state)
                0: begin
                    lcd_ce <= 0;
                    lcd_dc <= (index >= 8);
                    lcd_din <= data_byte[7];
                    shift <= data_byte;
                    bit_no <= 7;
                    state <= 1;
                end

                1: begin
                    lcd_clk <= 1;
                    state <= 2;
                end

                2: begin
                    lcd_clk <= 0;
                    if (bit_no == 0)
                        state <= 3;
                    else begin
                        bit_no <= bit_no - 1'b1;
                        lcd_din <= shift[6];
                        shift <= {shift[6:0],1'b0};
                        state <= 1;
                    end
                end

                3: begin
                    lcd_ce <= 1;
                    if (index == 511)
                        done <= 1;
                    else
                        index <= index + 1'b1;
                    state <= 0;
                end
            endcase
        end
        else
            div <= div + 1'b1;
    end

endmodule
