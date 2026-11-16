

module LCD5110 (
    input  wire clk,
    output reg lcd_rst = 0,
    output reg lcd_ce  = 1,
    output reg lcd_dc  = 0,
    output reg lcd_din = 0,
    output reg lcd_clk = 0
);

    // Nokia 5110 - PCD8544
    // FPGA clock: 50 MHz

    function [7:0] init_byte;
        input [2:0] n;
        begin
            case (n)
                0: init_byte = 8'h21;
                1: init_byte = 8'hB8;
                2: init_byte = 8'h04;
                3: init_byte = 8'h14;
                4: init_byte = 8'h20;
                5: init_byte = 8'h0C;
                6: init_byte = 8'h80;
                7: init_byte = 8'h40;
                default: init_byte = 8'h00;
            endcase
        end
    endfunction

    // Font 5x7: GAGNIUC
    function [7:0] text_byte;
        input [5:0] n;
        begin
            case (n)

                // G
                 0: text_byte = 8'h3E;
                 1: text_byte = 8'h41;
                 2: text_byte = 8'h49;
                 3: text_byte = 8'h49;
                 4: text_byte = 8'h7A;

                // A
                 6: text_byte = 8'h7E;
                 7: text_byte = 8'h11;
                 8: text_byte = 8'h11;
                 9: text_byte = 8'h11;
                10: text_byte = 8'h7E;

                // G
                12: text_byte = 8'h3E;
                13: text_byte = 8'h41;
                14: text_byte = 8'h49;
                15: text_byte = 8'h49;
                16: text_byte = 8'h7A;

                // N
                18: text_byte = 8'h7F;
                19: text_byte = 8'h02;
                20: text_byte = 8'h0C;
                21: text_byte = 8'h10;
                22: text_byte = 8'h7F;

                // I
                24: text_byte = 8'h00;
                25: text_byte = 8'h41;
                26: text_byte = 8'h7F;
                27: text_byte = 8'h41;
                28: text_byte = 8'h00;

                // U
                30: text_byte = 8'h3F;
                31: text_byte = 8'h40;
                32: text_byte = 8'h40;
                33: text_byte = 8'h40;
                34: text_byte = 8'h3F;

                // C
                36: text_byte = 8'h3E;
                37: text_byte = 8'h41;
                38: text_byte = 8'h41;
                39: text_byte = 8'h41;
                40: text_byte = 8'h22;

                // Spatii intre litere
                default: text_byte = 8'h00;

            endcase
        end
    endfunction

    // 0 = initializare
    // 1 = stergere ecran
    // 2 = pozitionare cursor
    // 3 = GAGNIUC

    reg [1:0] phase = 0;
    reg [9:0] pos = 0;

    wire [7:0] tx_byte =
        (phase == 0) ? init_byte(pos[2:0]) :
        (phase == 1) ? 8'h00 :
        (phase == 2) ?
            ((pos == 0) ? 8'h95 : 8'h42) :
        text_byte(pos[5:0]);

    localparam RESET_LOW  = 0;
    localparam RESET_HIGH = 1;
    localparam LOAD       = 2;
    localparam CLOCK_UP   = 3;
    localparam CLOCK_DOWN = 4;
    localparam BYTE_END   = 5;
    localparam ADVANCE    = 6;
    localparam DONE       = 7;

    reg [3:0] state = RESET_LOW;
    reg [21:0] timer = 0;
    reg [5:0] spi_div = 0;
    reg [7:0] shift_byte = 0;
    reg [2:0] bit_index = 0;

    always @(posedge clk) begin
        case (state)

            RESET_LOW: begin
                lcd_rst <= 0;
                lcd_ce <= 1;
                lcd_clk <= 0;

                if (timer == 22'd999999) begin
                    timer <= 0;
                    lcd_rst <= 1;
                    state <= RESET_HIGH;
                end
                else
                    timer <= timer + 1'b1;
            end

            RESET_HIGH: begin
                if (timer == 22'd2499999) begin
                    timer <= 0;
                    state <= LOAD;
                end
                else
                    timer <= timer + 1'b1;
            end

            LOAD: begin
                lcd_ce <= 0;
                lcd_dc <= (phase == 1 ||
                           phase == 3);
                lcd_clk <= 0;
                lcd_din <= tx_byte[7];

                shift_byte <= tx_byte;
                bit_index <= 7;
                spi_div <= 0;
                state <= CLOCK_UP;
            end

            CLOCK_UP: begin
                if (spi_div == 49) begin
                    spi_div <= 0;
                    lcd_clk <= 1;
                    state <= CLOCK_DOWN;
                end
                else
                    spi_div <= spi_div + 1'b1;
            end

            CLOCK_DOWN: begin
                if (spi_div == 49) begin
                    spi_div <= 0;
                    lcd_clk <= 0;

                    if (bit_index == 0)
                        state <= BYTE_END;
                    else begin
                        bit_index <= bit_index - 1'b1;
                        lcd_din <= shift_byte[6];
                        shift_byte <=
                            {shift_byte[6:0], 1'b0};
                        state <= CLOCK_UP;
                    end
                end
                else
                    spi_div <= spi_div + 1'b1;
            end

            BYTE_END: begin
                lcd_ce <= 1;
                state <= ADVANCE;
            end

            ADVANCE: begin

                if (phase == 3 && pos == 41)
                    state <= DONE;

                else begin
                    state <= LOAD;

                    if (phase == 0 && pos == 7) begin
                        phase <= 1;
                        pos <= 0;
                    end

                    else if (phase == 1 &&
                             pos == 503) begin
                        phase <= 2;
                        pos <= 0;
                    end

                    else if (phase == 2 &&
                             pos == 1) begin
                        phase <= 3;
                        pos <= 0;
                    end

                    else
                        pos <= pos + 1'b1;
                end
            end

            DONE: begin
                lcd_ce <= 1;
                lcd_clk <= 0;
            end

            default:
                state <= RESET_LOW;

        endcase
    end

endmodule
