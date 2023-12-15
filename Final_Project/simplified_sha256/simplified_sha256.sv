module simplified_sha256 #(parameter integer NUM_OF_WORDS = 40)(
 input logic  clk, rst_n, start,
 input logic  [15:0] input_addr, hash_addr,
 output logic done, memory_clk, enable_write,
 output logic [15:0] memory_addr,
 output logic [31:0] memory_write_data,
 input logic [31:0] memory_read_data);

// FSM state variables 
enum logic [2:0] {IDLE, BLOCK, COMPUTE, WRITE} state;

parameter integer SIZE = NUM_OF_WORDS * 32; 

// NOTE : Below mentioned frame work is for reference purpose.
// Local variables might not be complete and you might have to add more variables
// or modify these variables. Code below is more as a reference.

// Local variables
logic [31:0] w[64];
logic [31:0] message[16 * ((NUM_OF_WORDS + 2) / 16 + (((NUM_OF_WORDS+2)%16) > 0))];
logic [31:0] wt;
logic [31:0] S0,S1;
logic [31:0] hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7;
logic [31:0] A, B, C, D, E, F, G, H;
logic [ 7:0] i, j; 
logic [15:0] offset; // in word address
logic [ 7:0] num_blocks;
logic        enable_write;
logic [15:0] present_addr;
logic [31:0] present_write_data;
logic [512:0] data_read;
logic [ 7:0] tstep;




// SHA256 K constants
parameter int k[0:63] = '{
   32'h428a2f98,32'h71374491,32'hb5c0fbcf,32'he9b5dba5,32'h3956c25b,32'h59f111f1,32'h923f82a4,32'hab1c5ed5,
   32'hd807aa98,32'h12835b01,32'h243185be,32'h550c7dc3,32'h72be5d74,32'h80deb1fe,32'h9bdc06a7,32'hc19bf174,
   32'he49b69c1,32'hefbe4786,32'h0fc19dc6,32'h240ca1cc,32'h2de92c6f,32'h4a7484aa,32'h5cb0a9dc,32'h76f988da,
   32'h983e5152,32'ha831c66d,32'hb00327c8,32'hbf597fc7,32'hc6e00bf3,32'hd5a79147,32'h06ca6351,32'h14292967,
   32'h27b70a85,32'h2e1b2138,32'h4d2c6dfc,32'h53380d13,32'h650a7354,32'h766a0abb,32'h81c2c92e,32'h92722c85,
   32'ha2bfe8a1,32'ha81a664b,32'hc24b8b70,32'hc76c51a3,32'hd192e819,32'hd6990624,32'hf40e3585,32'h106aa070,
   32'h19a4c116,32'h1e376c08,32'h2748774c,32'h34b0bcb5,32'h391c0cb3,32'h4ed8aa4a,32'h5b9cca4f,32'h682e6ff3,
   32'h748f82ee,32'h78a5636f,32'h84c87814,32'h8cc70208,32'h90befffa,32'ha4506ceb,32'hbef9a3f7,32'hc67178f2
};


// Generate request to memory
// for reading from memory to get original message
// for writing final computed has value
assign memory_clk = clk;
assign memory_addr = present_addr + next_offset;
assign memory_we = enable_write;
assign memory_write_data = present_write_data;


assign num_blocks = determine_num_blocks(NUM_OF_WORDS); 
assign tstep = (i - 1);

// Note : Function defined are for reference purpose. Feel free to add more functions or modify below.
// Function to determine number of blocks in memory to fetch
function logic [15:0] determine_num_blocks(input logic [31:0] size);
  // Student to add function implementation
	begin
		determine_num_blocks = (size + 2) / 16 + 1;
	end
endfunction


// SHA256 hash round
function logic [255:0] sha256_op(input logic [31:0] a, b, c, d, e, f, g, h, w,
                                 input logic [7:0] t);
    logic [31:0] S1, S0, ch, maj, t1, t2; // internal signals
	begin
		S0 = ror(a, 2) ^ ror(a, 13) ^ ror(a, 22);
		maj = (a & b) ^ (a & c) ^ (b ^ c);
		t2 = S0 + maj;

		S1 = ror(e, 6) ^ ror(e, 11) ^ ror(e, 25);
		ch = (e & f) ^ ((~e) & g);
		t1 = h + S1 + ch + k[t] + w;
		
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


always_ff @(posedge clk, negedge rst_n)
begin
  if (!rst_n) begin
    state <= IDLE;

  end 
  else begin 
	// SHA-256 FSM 
	// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function
	// and write back hash value back to memory
	case (state)
		// Initialize hash values h0 to h7 and a to h, other variables and memory we, address offset, etc
		IDLE: begin 
			if(start) begin 
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
				
				i <= '0; 
				j <= '0;
				enable_write <= '0;
				offset <= '0;
				num_blocks <= '0;
				state <= WAIT;

		end
		end

		// SHA-256 FSM 
		// Get a BLOCK from the memory, COMPUTE Hash output using SHA256 function    
		// and write back hash value back to memory
		BLOCK: begin
			if(num_blocks > '0) begin
				present_addr = input_addr + offset;
				state <= COMPUTE;
			end else begin 
				state <= WRITE;
			end
		// Fetch message in 512-bit block size
		// For each of 512-bit block initiate hash value computation

		end
		
		// For each block compute hash function
		// Go back to BLOCK stage after each block hash computation is completed and if
		// there are still number of message blocks available in memory otherwise
		// move to WRITE stage
		COMPUTE: begin
		// 64 processing rounds steps for 512-bit block
		
			{A,B,C,D,E,F,G,H} = {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7};
			for(i = 0; i < 64; i = i + 1) begin
				if(i < 16) begin // since we use hex
					w[i] = message[i];
				end else begin
					S0 = ror(w[i-15], 7) ^ ror(w[i-15], 18) ^ ror(w[i-15], 3);
					S1 = ror(w[i-2], 17) ^ ror(w[i-2], 19) ^ ror(w[i-2], 10);
					w[i] = w[i-16] + S0 + w[i-7] + S1;
				end

				{A,B,C,D,E,F,G,H} = sha256_op(A,B,C,D,E,F,G,H, w[i], i);

				{hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7} = {hash0 + A, hash1 + B, hash2 + C, hash3 + D, hash4 + E, hash5 + F, hash6 + G, hash7 + H}
			end

			state = (num_blocks > '0) ? BLOCK : WRITE;
					
		end

		// h0 to h7 each are 32 bit hashes, which makes up total 256 bit value
		// h0 to h7 after compute stage has final computed hash value
		// write back these h0 to h7 to memory starting from output_addr
		WRITE: begin
			present_addr <= hash_addr;
			present_write_data <= {hash0, hash1, hash2, hash3, hash4, hash5, hash6, hash7};
			enable_write <= 'h1;
			state <= IDLE;
		end
	endcase
	end
end 

// Generate done when SHA256 hash computation has finished and moved to IDLE state
assign done = (state == IDLE);

endmodule
