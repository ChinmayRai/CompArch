library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

Entity DP_Group is
Port( 
       instruction: in std_logic_vector(31 downto 0); -- 32 bit instruction set
	  PC: inout std_logic_vector(31 downto 0);
	  flag: inout std_logic_vector (3 downto 0);
	  clock:in std_logic;
	  reset:in std_logic
	);
end DP_Group;


Architecture Behavioral of DP_Group is
Component ALU
Port (
    A, B     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    carry    : in std_logic_vector(31 downto 0);
    ALU_Sel  : in  STD_LOGIC_VECTOR(3 downto 0);  -- input 4-bit for selecting function
    ALU_out  : out  STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    flag     : out std_logic_vector(3 downto 0)        -- CNZV flags
    );
END Component;

Component multiplier is
    Port (
    A,B          : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    ALU_out      : out STD_LOGIC_VECTOR(31 downto 0) --  output 31-bit 
    );
end Component; 

Component shifter is
    Port (
    A           : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    shift_type  : in std_logic_vector(1 downto 0);
    shiftamt    : in STD_LOGIC_VECTOR(4 downto 0);  -- input 5-bit for selecting function
    ALU_out     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    shift_carry : out std_logic --shiter carry
    );
end Component;

Component chull_register is
	Port(
		write_data : in std_logic_vector(31 downto 0); -- data to be written
		read_add1,read_add2 : in std_logic_vector(3 downto 0); -- 4-bit read address for read ports
		write_add  : in std_logic_vector(3 downto 0); -- 4-bit address for writing
		clock, reset, write_enable : in std_logic;
		read_data1,read_data2  : out std_logic_vector(31 downto 0)
	); 
end Component;



Signal source_binary: std_logic_vector (3 downto 0); -- specifies the bits for source register (Rn)
Signal destination_binary: std_logic_vector (3 downto 0); -- bits of destination register (Rd)
Signal shift_binary: std_logic_vector(3 downto 0); --bits of shift register (Rs)
Signal rotate_binary: std_logic_vector (3 downto 0); -- rotate register (Rm)
Signal temp_flag: std_logic_vector(3 downto 0);
Signal final_answer: std_logic_vector (3 downto 0); --carries the final answer of the dp group
Signal write_to_register: std_logic_vector(31 downto 0);
Signal temp_source: std_logic_vector (31 downto 0);
Signal temp_last: std_logic_vector (31 downto 0);
Signal temp_shift_amt: std_logic_vector (31 downto 0);
Signal thirty_two_zeros: std_logic_vector (31 downto 0) := "00000000000000000000000000000000";
Signal shift_out_constant: std_logic_vector (31 downto 0);
Signal shift_out_register: std_logic_vector (31 downto 0);
Signal shift_carry_out_constant:std_logic;
Signal shift_carry_out_register: std_logic;
Signal shift_carry:std_logic;
Signal temp_enable: std_logic:='0';
Signal temp_ALU_OUT: std_logic_vector (31 downto 0);
Signal temp_temp_flag: std_logic_vector (3 downto 0);
Signal temp_source_rotation: std_logic_vector(31 downto 0);
Signal rot_out: std_logic_vector(31 downto 0);
Signal rot_carry: std_logic;
Signal A: std_logic_vector(31 downto 0);
Signal B: std_logic_vector(31 downto 0);
Signal temp_out:std_logic_vector(31 downto 0);
Signal op2_mul:std_logic_vector(31 downto 0);
Signal opcode:std_logic_vector(4 downto 0)
BEGIN

--read what value is in Rn
read_source: chull_register PORT MAP(write_to_register,instruction(19 downto 16), instruction(11 downto 8),instruction (15 downto 12),clock,reset,'0',temp_source,temp_shift_amt);
write_destination: chull_register PORT MAP(write_to_register,instruction(19 downto 16), instruction(11 downto 8),instruction (15 downto 12),clock,reset,temp_enable,temp_source,temp_shift_amt);
--read what value is in last register
read_last: chull_register PORT MAP(write_to_register,instruction(3 downto 0), instruction(19 downto 16),instruction (15 downto 12),clock,reset,'0',temp_last,temp_source);

shifter_constant:shifter PORT MAP (temp_last,instruction (6 downto 5),instruction (11 downto 7), shift_out_constant, shift_carry_out_constant);

shifter_register: shifter PORT MAP(temp_last, instruction (6 downto 5), temp_shift_amt(4 downto 0),shift_out_register, shift_carry_out_register);

--How to do rotate?
rot: shifter PORT MAP (temp_source_rotation,"11",temp_shift_amt,rot_out,rot_carry);

chull_alu: ALU PORT MAP(A,B,thirty_two_zeros,opcode,temp_ALU_out,flag);

chull_mul: multiply PORT MAP(temp_source,temp_shift_amt,temp_out);

temp_source_rotation (31 downto 8) <= thirty_two_zeros(31 downto 8);
temp_source_rotation (7 downto 0) <= instruction (7 downto 0);
temp_shift_amt(4 downto 1) <= instruction (11 downto 8);
temp_shift_amt (0) <= '0';
--F==11 for mul??
Process(instruction,PC)
BEGIN
	PC <= PC+4;
	if ((instruction(27 downto 26))="00") then --DP
	    A <= temp_source; 
		opcode <= instruction(24 downto 21);
		if(instruction(25)='0') then
			if(instruction(4)='0') then
				B <= shift_out_constant;
			else
				B <= shift_out_register;
			end if;
		else
			B <= rot_out;
		end if;	
	
		if () then --condition for flags
			temp_temp_flag <= temp_flag;
		else
			temp_temp_flag <= flag;
		end if;
		
	elsif((instruction(27 downto 26))="11") then --multiply
		if(instruction(21) = '1') then --mla
			A <= temp_out;
			B <= temp_last;
			opcode <= "0100";
			temp_out <= temp_ALU_out;
		end if;
		temp_ALU_out <= temp_out; --gives output of both mul and mla in temp_ALU_out
	end if;	
	
	write_to_register <= temp_ALU_out;
		if () then --condition for writing in register
			temp_enable <= '1';
		else
			temp_enable <= '0';
		end if;		
		
		
END Process;

flag <= temp_temp_flag;

END Behavioral;
