module bitcoin_hash (input logic        clk, reset_n, start,
                     input logic [15:0] header_addr, hash_out_addr,
                    output logic        done, mem_clk, mem_we,
                    output logic [15:0] memory_addr,
                    output logic [31:0] memory_write_data,
                     input logic [31:0] memory_read_data);

// Student to add rest of the code here
parameter NUM_NONCES = 16;

assign mem_clk = clk; 
enum logic [2:0] {IDLE, BLOCK, BUFFER, BUFFER2, READ4, COMPUTE, WRITE} state;
logic [31:0] A[NUM_NONCES],B[NUM_NONCES],C[NUM_NONCES],D[NUM_NONCES],E[NUM_NONCES],F[NUM_NONCES],G[NUM_NONCES],H[NUM_NONCES], wt[NUM_NONCES];
logic [31:0] w[NUM_NONCES][16];
logic [31:0] hash0[NUM_NONCES], hash1[NUM_NONCES], hash2[NUM_NONCES], hash3[NUM_NONCES], hash4[NUM_NONCES], hash5[NUM_NONCES], hash6[NUM_NONCES], hash7[NUM_NONCES];
logic [31:0] nonce, t, j;
logic [15:0] curr_word;
logic [1:0] flag, initFlag;


parameter int k[64] = '{
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


function logic [31:0] wtnew(input int n); //no inputs
  logic [31:0] s0, s1;
  s0 = ror(w[n][1],7)^ror(w[n][1],18)^(w[n][1]>>3);
  s1 = ror(w[n][14],17)^ror(w[n][14],19)^(w[n][14]>>10);
  wtnew = w[n][0]+s0+w[n][9]+s1;
endfunction 


always_ff @(posedge clk, negedge reset_n)
begin
  if (!reset_n) begin
    state <= IDLE;
  end
  else begin
    case (state)
      IDLE: begin
        if (start) begin 
            mem_we <= 0;
            memory_addr <= header_addr;
            curr_word <= 1; //word counter
            t <= 0; 
            j <= 0; 
            nonce <= 0;
            flag <= 0;
            initFlag <= 0;
            for (int n = 0; n < NUM_NONCES; n++) begin
              H[n][0] <= 32'h6a09e667;
              H[n][1] <= 32'hbb67ae85;
              H[n][2] <= 32'h3c6ef372;
              H[n][3] <= 32'ha54ff53a;
              H[n][4] <= 32'h510e527f;
              H[n][5] <= 32'h9b05688c;
              H[n][6] <= 32'h1f83d9ab;
              H[n][7] <= 32'h5be0cd19;
            end
            state <= BLOCK;
        end
      end
      BLOCK: begin
        if (initFlag == 0) begin
          memory_addr <= header_addr + curr_word; //Read (j)th word from message
          curr_word <= curr_word + 1; //Updatemto read next word
          initFlag <= 1;
        end
        //mem_we <= 0; 
        else begin
          memory_addr <= header_addr + curr_word; 
          curr_word <= curr_word + 1;
          j <= j + 1; 
          state <= BUFFER;
          for (int n=0; n < NUM_NONCES; n++) begin
            w[n][j] <= memory_read_data; 
          end
        end
      end
      BUFFER: begin 
        for (int n = 0; n < NUM_NONCES; n++) begin
            w[n][j] <= memory_read_data;
        end
        j <= j + 1;

        if (curr_word == 21) begin // If all words read, go to COMPUTEUTEUTEUTE
          for (int n = 0; n < NUM_NONCES; n++) begin
            w[n][j] <= memory_read_data;
            state <= COMPUTE;
            // Added Padded words to w array
            w[n][3] <= n;
            for (int i = 4; i < 16; i++) begin
              if (i == 4) begin
                w[n][i] <= 32'h80000000;
              end
              else if (i == 15) begin
                w[n][i] <= 32'd640;
              end
              else begin
                w[n][i] <= 32'h00000000;
              end
            end
            wt[n] <= w[n][0]; 
            {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {hash0[n], hash1[n], hash2[n], hash3[n], hash4[n], hash5[n], hash6[n], hash7[n]};
          end
          t <= t + 1; 
          flag <= 1;
        end
        else if (j == 15) begin 
          state <= COMPUTE;
          j <= 0;
          t <= t + 1;
          for (int n = 0; n < NUM_NONCES; n++) begin
            wt[n] <= w[n][0]; 
            {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {hash0[n], hash1[n], hash2[n], hash3[n], hash4[n], hash5[n], hash6[n], hash7[n]};
          end
        end
        else begin
          state <= BLOCK;
          memory_addr <= header_addr + curr_word;
          curr_word <= curr_word + 1;
        end
      end
      READ4: begin //Get new message for "Second" SHA256 Function
        //Padded Words
        for (int n = 0; n < NUM_NONCES; n++) begin
          for (int i = 8; i < 16; i++) begin
              if (i == 8) begin
                w[n][i] <= 32'h80000000;
              end
              else if (i == 15) begin
                w[n][i] <= 32'd256;
              end
              else begin
                w[n][i] <= 32'h00000000;
              end
          end
          state <= COMPUTE;
          wt[n] <= w[n][0];
		  	 {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= {hash0[n], hash1[n], hash2[n], hash3[n], hash4[n], hash5[n], hash6[n], hash7[n]};       
			end
        t <= t + 1;
      end  
      COMPUTE: begin 
        if (t < 65) begin
          for (int n = 0; n < NUM_NONCES; n++) begin
            if (t < 16) begin
              wt[n] <= w[n][t];
            end
            else begin
              wt[n] <= wtnew(n);
              for (int i = 0; i<15;i++) begin
                w[n][i] <= w[n][i+1];
              end
              w[n][15] <= wtnew(n);
            end
            {A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n]} <= sha256_op(A[n], B[n], C[n], D[n], E[n], F[n], G[n], H[n], wt[n], k[t-1]);
          end
          t <= t + 1;
        end
        else begin 
          if (flag == 1) begin 
            state <= READ4; 
            t <= 0; 
            // Reintialize constants for 2nd SHA
            for (int n = 0; n < NUM_NONCES; n++) begin
              {w[n][0], w[n][1], w[n][2], w[n][3], w[n][4], w[n][5], w[n][6], w[n][7]} <= {hash0[n]+A[n], hash1[n]+B[n], hash2[n]+C[n], hash3[n]+D[n], hash4[n]+E[n], hash5[n]+F[n], hash6[n]+G[n], hash7[n]+H[n]};
              hash0[n] <= 32'h6a09e667;
              hash1[n] <= 32'hbb67ae85;
              hash2[n] <= 32'h3c6ef372;
              hash3[n] <= 32'ha54ff53a;
              hash4[n] <= 32'h510e527f;
              hash5[n] <= 32'h9b05688c;
              hash6[n] <= 32'h1f83d9ab;
              hash7[n] <= 32'h5be0cd19;
            end
            flag <= 0;
          end
          else if (curr_word < 21) begin // Begin Second Block for First SHA
            state <= BLOCK;
            t <= 0;
            for (int n = 0; n < NUM_NONCES; n++) begin
              {hash0[n], hash1[n], hash2[n], hash3[n], hash4[n], hash5[n], hash6[n], hash7[n]} <= {hash0[n]+A[n], hash1[n]+B[n], hash2[n]+C[n], hash3[n]+D[n], hash4[n]+E[n], hash5[n]+F[n], hash6[n]+G[n], hash7[n]+H[n]};
            end
            initFlag <= 0;
          end
          else begin
            for (int n = 0; n < NUM_NONCES; n++) begin
              {hash0[n], hash1[n], hash2[n], hash3[n], hash4[n], hash5[n], hash6[n], hash7[n]} <= {hash0[n]+A[n], hash1[n]+B[n], hash2[n]+C[n], hash3[n]+D[n], hash4[n]+E[n], hash5[n]+F[n], hash6[n]+G[n], hash7[n]+H[n]};
            end
            state <= WRITE;
          end
        end
      end

      WRITE: begin
        mem_we <= 1;
        if (nonce <= NUM_NONCES) begin
          flag <= 1;
          memory_addr <= hash_out_addr + nonce;
          memory_write_data <= H[nonce][0];
          nonce <= nonce + 1;
        end
        else begin
          done <= 1;
          state <= IDLE;
        end
      end
  endcase
  end
end
endmodule   