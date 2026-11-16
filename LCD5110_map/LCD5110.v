module LCD5110 (
    input  wire clk,
    output reg lcd_rst = 0,
    output reg lcd_ce  = 1,
    output reg lcd_dc  = 0,
    output reg lcd_din = 0,
    output reg lcd_clk = 0
);
    // Nokia 5110 PCD8544, FPGA 50 MHz.
    // Place GAGNIUC_84x48.hex next to this .v file in the Quartus project.
    // 84 * 48 = 4032 pixels, packed in 504 bytes (6 banks x 84 columns).

    reg [7:0] bitmap [0:503];
    initial $readmemh("GAGNIUC_84x48.hex", bitmap);

    function [7:0] init_byte;
        input [2:0] n;
        begin
            case (n)
                0: init_byte = 8'h21;  // Extended instruction set
                1: init_byte = 8'hB8;  // Vop / contrast (adjust as needed)
                2: init_byte = 8'h04;  // Temperature coefficient
                3: init_byte = 8'h14;  // Bias
                4: init_byte = 8'h20;  // Basic instruction set
                5: init_byte = 8'h0C;  // Normal display mode
                6: init_byte = 8'h80;  // X = 0
                7: init_byte = 8'h40;  // Y bank = 0
                default: init_byte = 8'h00;
            endcase
        end
    endfunction

    reg phase = 0;              // 0: init, 1: bitmap
    reg [9:0] pos = 0;         // 0..503
    wire [7:0] tx_byte = phase ? bitmap[pos] : init_byte(pos[2:0]);

    localparam RESET_LOW  = 4'd0;
    localparam RESET_HIGH = 4'd1;
    localparam LOAD       = 4'd2;
    localparam CLOCK_UP   = 4'd3;
    localparam CLOCK_DOWN = 4'd4;
    localparam BYTE_END   = 4'd5;
    localparam ADVANCE    = 4'd6;
    localparam DONE       = 4'd7;

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
                end else timer <= timer + 1'b1;
            end
            RESET_HIGH: begin
                if (timer == 22'd2499999) begin
                    timer <= 0;
                    state <= LOAD;
                end else timer <= timer + 1'b1;
            end
            LOAD: begin
                lcd_ce <= 0;
                lcd_dc <= phase;
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
                end else spi_div <= spi_div + 1'b1;
            end
            CLOCK_DOWN: begin
                if (spi_div == 49) begin
                    spi_div <= 0;
                    lcd_clk <= 0;
                    if (bit_index == 0) state <= BYTE_END;
                    else begin
                        bit_index <= bit_index - 1'b1;
                        lcd_din <= shift_byte[6];
                        shift_byte <= {shift_byte[6:0],1'b0};
                        state <= CLOCK_UP;
                    end
                end else spi_div <= spi_div + 1'b1;
            end
            BYTE_END: begin
                lcd_ce <= 1;
                state <= ADVANCE;
            end
            ADVANCE: begin
                if (phase && pos == 10'd503)
                    state <= DONE;
                else begin
                    if (!phase && pos == 10'd7) begin
                        phase <= 1;
                        pos <= 0;
                    end else pos <= pos + 1'b1;
                    state <= LOAD;
                end
            end
            DONE: begin
                lcd_ce <= 1;
                lcd_clk <= 0;
            end
            default: state <= RESET_LOW;
        endcase
    end
endmodule
