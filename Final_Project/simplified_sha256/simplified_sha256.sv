module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start,
 input logic  [15:0] input_addr, hash_addr,
 output logic done, memory_clk, enable_write,
 output logic [15:0] memory_addr,
 output logic [31:0] memory_write_data,
 input logic [31:0] memory_read_data
 );

enum logic [2:0] {IDLE, BLOCK, BUFFER, BUFFER2, READ, COMPUTE, WRITE} state;
logic [31:0] temp,wt;
logic [31:0] w[16];
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H; 
logic [31:0] t, j;
logic [15:0] curr_word;
logic [ 7:0] tstep;
logic [31:0] present_write_data;
 
 
parameter int k[0:63] = '{
   32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5, 32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
   32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3, 32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
   32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc, 32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
   32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7, 32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
   32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13, 32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
   32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3, 32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
   32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5, 32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
   32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208, 32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
};

// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign memory_clk = clk;
assign memory_write_data = present_write_data;

assign num_blocks = determine_num_blocks(NUM_OF_WORDS);


// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);

  // Student to add function implementation
	if(size == 20) begin 
		determine_num_blocks = 2;
	end 
	if(size == 30) begin 
		determine_num_blocks = 3;
	end 
	if(size == 40) begin 
		determine_num_blocks = 3;
	end 
endfunction


function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w, k);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
begin
    S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
    ch = (e & f) ^ ((~e) & g);
    t1 = h + S1 + ch + k + w;
    S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
    maj = (a & b) ^ (a & c) ^ (b & c);
    t2 = S0 + maj;

    sha256_op = {t1 + t2, a, b, c, d + t1, e, f, g};
end
endfunction

// Right Rotation Example : right rotate input x by r
// Lets say input x = 1111 ffff 2222 3333 4444 6666 7777 8888
// lets say r = 4
// x >> r  will result in : 0000 1111 ffff 2222 3333 4444 6666 7777 
// x << (32-r) will result in : 8888 0000 0000 0000 0000 0000 0000 0000
// final right rotate expression is = (x >> r) | (x << (32-r));
// (0000 1111 ffff 2222 3333 4444 6666 7777) | (8888 0000 0000 0000 0000 0000 0000 0000)
// final value after right rotate = 8888 1111 ffff 2222 3333 4444 6666 7777
// Right rotation function

function logic [31:0] ror(input logic [31:0] in,
                                  input logic [7:0] s);
	begin
	ror = (in >> s) | (in << (32-s));
	end
endfunction


function logic [31:0] wtnew; //no inputs
  logic [31:0] s0, s1;
 begin
  s0 = ror(w[1],7) ^ ror(w[1],18) ^ (w[1]>>3);
  s1 = ror(w[14],17) ^ ror(w[14],19) ^ (w[14]>>10);
  wtnew = w[0] + s0 + w[9] + s1;
 end
endfunction


always_ff @(posedge clk, negedge rst_n)
begin
  if (!rst_n) begin
    state <= IDLE;
  end
  else begin
    case (state)
      IDLE: begin
        if (start) begin 
			    enable_write <= 0;
				 curr_word <= 0; //word counter
				 t <= 0; // counter for w for COMPUTEUTEUTEUTE
				 j <= 0; // counter for w for read words from message
				 tstep <= 0;

            //init h0-h7
				hash0 <= 32'h6a09e667;
				hash1 <= 32'hbb67ae85;
				hash2 <= 32'h3c6ef372;
				hash3 <= 32'ha54ff53a;
				hash4 <= 32'h510e527f;
				hash5 <= 32'h9b05688c;
				hash6 <= 32'h1f83d9ab;
				hash7 <= 32'h5be0cd19;
				//init a-h
				A <= 32'h6a09e667;
				B <= 32'hbb67ae85;
				C <= 32'h3c6ef372;
				D <= 32'ha54ff53a;
				E <= 32'h510e527f;
				F <= 32'h9b05688c;
				G <= 32'h1f83d9ab;
				H <= 32'h5be0cd19;
          state <= BLOCK;
        end
      end
      BLOCK: begin
        enable_write <= 0; 
        memory_addr <= input_addr + curr_word; 
        curr_word <= curr_word + 1; 
        state <= BUFFER;
      end
      BUFFER: begin
        state <= BUFFER2;
        memory_addr <= input_addr + curr_word;
        curr_word <= curr_word + 1;
      end
      BUFFER2: begin 
        w[j] <= memory_read_data; 
        j <= j + 1;	
        state <= READ;
      end
      READ: begin
        w[j] <= memory_read_data; 
        if (curr_word == NUM_OF_WORDS) begin
          state <= COMPUTE;
			 // Added Padded words to w array
			 w[(NUM_OF_WORDS%16)] = 32'h80000000;
			 for (int x = (NUM_OF_WORDS%16)+1; x < 15; x = x+1) begin
				  w[x] <= 32'h00000000;
			 end
			 w[15] <= NUM_OF_WORDS*32;
          wt <= w[0]; 
          {A,B,C,D,E,F,G,H} <= {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7};
          t <= t + 1; 
        end
        else if (curr_word == 16) begin 
          state <= COMPUTE;
          j <= 0; 
			 wt <= w[0];
          t <= t + 1;
          {A,B,C,D,E,F,G,H} <= {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7};
        end
        else begin
          state <= BLOCK;
          j <= j + 1;
        end
      end

      COMPUTE: begin 
        if (t < 65) begin
          if (t < 16) begin
            wt <= w[t];
          end
          else begin
            wt <= wtnew();
            for (int i = 0; i<15;i++) begin
              w[i] <= w[i+1];
            end
            w[15] <= wtnew();
          end
          {A,B,C,D,E,F,G,H} <= sha256_op(A,B,C,D,E,F,G,H, wt, k[t-1]);

          t <= t + 1;
        end
        else begin 
          hash0 <= hash0 + A;
			 hash1 <= hash1 + B;
			 hash2 <= hash2 + C;
			 hash3 <= hash3 + D;
			 hash4 <= hash4 + E;
			 hash5 <= hash5 + F;
			 hash6 <= hash6 + G;
			 hash7 <= hash7 + H;
          if (curr_word == NUM_OF_WORDS) begin 
				enable_write <= 1;
            state <= WRITE;
          end
          else begin
            state <= BLOCK;
            t <= 0;
          end
        end
      end

      WRITE: begin
			case (tstep)
				0: begin
					memory_addr <= hash_addr;
					present_write_data <= hash0;
				end
				1: begin
					memory_addr <= hash_addr + 1;
					present_write_data <= hash1;
				end
				2: begin
					memory_addr <= hash_addr + 2;
					present_write_data <= hash2;
				end
				3: begin
					memory_addr <= hash_addr + 3;
					present_write_data <= hash3;
				end
				4: begin
					memory_addr <= hash_addr + 4;
					present_write_data <= hash4;
				end
				5: begin
					memory_addr <= hash_addr + 5;
					present_write_data <= hash5;
				end
				6: begin
					memory_addr <= hash_addr + 6;
					present_write_data <= hash6;
				end
				7: begin
					memory_addr <= hash_addr + 7;
					present_write_data <= hash7;
				end
				default: begin
					state <= IDLE;
			   end
			 endcase

			 tstep <= tstep + 1;

			 // Transition to IDLE state when all hash values are written
			 if (tstep == 8) begin
				  state <= IDLE;
				  tstep <= 'b0; // Reset the counter for the next computation
			 end
		end
		default: begin
			state <= IDLE;
		end
  endcase
  end
end

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule   