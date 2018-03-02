
----------------------------------------------------ALU-----------------------------------------------------
-------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity ALU is
    Port (
    A, B     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    carry    : in std_logic;
    ALU_Sel  : in  STD_LOGIC_VECTOR(3 downto 0);  -- input 4-bit for selecting function
    ALU_out  : out  STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    flag     : out std_logic_vector(3 downto 0)        -- CNZV flags
    );
end ALU; 

architecture Behavioral of ALU is

signal ALU_Result : std_logic_vector (31 downto 0);
signal c31,c32: std_logic;

begin
   process(A,B,ALU_Sel)
 begin
  case(ALU_Sel) is
  when "0000" => -- Logical AND
   ALU_Result <= A and B ; 
  when "0001" => -- Logical XOR
   ALU_Result <= A xor B ;
  when "0010" => -- Substraction
   ALU_Result <= A + (Not B) + 1 ;
  when "0011" => -- Reverse Subtract
   ALU_Result <=  (Not A) + B + 1 ;
  when "0100" => -- Additon
   ALU_Result <= A + B ;
  when "0101" => -- Addition with carry
   ALU_Result <= A + B + carry ;
  when "0110" => -- Substraction with carry
   ALU_Result <= A + (NOT B) + carry ;
  when "0111" => -- Reverse Subtract with Carryout
   ALU_Result <=  (Not A) + B + carry ;
  when "1000" => -- test
   ALU_Result <= A and B;
  when "1001" => -- test eq
   ALU_Result <= A xor B;
  when "1010" => -- cmp
   ALU_Result <= A + (Not B) + 1 ;
  when "1011" => -- cmn
   ALU_Result <= A + B;    
  when "1100" => -- Logical Or
   ALU_Result <= A or B;
  when "1101" => -- mov
   ALU_Result <= B ;
  when "1110" => -- bit clear
   ALU_Result <= A and (NOT B);
  when others => -- mvn 
   ALU_Result <= Not B;                
  end case;
 end process;

 flag(2) <= ALU_Result(31);
 WITH ALU_Result SELECT
 flag(1)<= '1' WHEN "00000000000000000000000000000000",
           '0' WHEN OTHERS;

c31 <= A(31) xor (B(31) xor ALU_Result(31));
c32 <= (A(31) and B(31)) or ((A(31) and c31) or(B(31) and c31));

flag(0) <= c31 xor c32;
flag(3) <= c32;

ALU_Out <= ALU_Result; -- ALU out
end Behavioral;
---------------------------------------------------------------------------------------------------------------------------





------------------------------------------------------------shifter--------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.CONV_STD_LOGIC_VECTOR;
use ieee.NUMERIC_STD.all;

entity shifter is
    Port (
    A           : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    shift_type  : in std_logic_vector(1 downto 0);
    shiftamt    : in STD_LOGIC_VECTOR(4 downto 0);  -- input 5-bit for selecting function
    shift_out     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    shift_carry : out std_logic --shifter carry
    );
end shifter; 

architecture Behavioral of shifter is
signal shift_Result : std_logic_vector (31 downto 0);
signal carry      : std_logic;
signal amt        : integer;

begin
amt <= to_integer(unsigned(shiftamt));
WITH shift_type SELECT
shift_Result <= STD_LOGIC_VECTOR(shift_left(unsigned(A),amt)) WHEN "00",
              STD_LOGIC_VECTOR(shift_right(unsigned(A),amt)) WHEN "01",
              STD_LOGIC_VECTOR(shift_right(signed(A), amt)) WHEN "10",
              STD_LOGIC_VECTOR(rotate_right(signed(A),amt)) WHEN OTHERS;
       
WITH shift_type SELECT           
carry <= A(32-amt) WHEN "00",
         A(amt-1) WHEN OTHERS ;
   
 shift_out <= shift_Result;
end Behavioral;
-------------------------------------------------------------------------------------------------------------------------------




---------------------------------------------------------------multiplier-----------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;
entity multiplier is
    Port (
    A,B          : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    mul_out      : out STD_LOGIC_VECTOR(31 downto 0) --  output 31-bit 
    );
end multiplier;  
architecture Behavioral of multiplier is
signal temp : STD_LOGIC_VECTOR(63 downto 0);
begin
temp <= A*B;
mul_out <= temp(31 downto 0);
end Behavioral;
------------------------------------------------------------------------------------------------------------------------------




-----------------------------------------------------------register-------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity registr is
  Port(
    write_data : in std_logic_vector(31 downto 0); -- data to be written
    read_add1,read_add2 : in std_logic_vector(3 downto 0); -- 4-bit read address for read ports
    write_add  : in std_logic_vector(3 downto 0); -- 4-bit address for writing
    clock, reset, write_enable : in std_logic;
    read_data1,read_data2  : out std_logic_vector(31 downto 0);
    PC : inout std_logic_vector(31 downto 0)
  ); 
end ;

architecture Behavioral of registr is
type regstr is array (0 to 15) of std_logic_vector(31 downto 0); 
signal reg : regstr;
signal indexr1,indexr2,indexw : integer;

begin
indexr1 <= to_integer(unsigned(read_add1));
indexr2 <= to_integer(unsigned(read_add2));
indexw  <= to_integer(unsigned(write_add));
read_data1 <= reg(indexr1);
read_data2 <= reg(indexr2);

  process(clock,reset)
  begin
  if (clock = '1' and clock'event) then
      if write_enable = '1' then
        reg(indexw) <= write_data;
    end if;
    if reset = '1' then
                PC <= "0";
        end if;
  end if ;

  end process;

end Behavioral;
----------------------------------------------------------------------------------------------------------------------------




---------------------------------------------------------------memory-processor_path---------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mp_path is
  Port(
    memory_in,processor_in : in std_logic_vector(31 downto 0); 
    instr: in std_logic_vector(2 downto 0); 
    byte_offset : in std_logic_vector(1 downto 0);
    memory_write_enable : out std_logic_vector(3 downto 0);
    memory_out,processor_out : out std_logic_vector(31 downto 0)
  ); 
end mp_path;

--000 : ldr
--001 : ldrh
--010 : ldrb
--011 : ldrsh
--100 : ldrsb
--101 : strb
--110 : strh
--111 : str

architecture Behavioral of mp_path is
signal out_val : std_logic_vector(31 downto 0);
constant sixteen_zero : std_logic_vector(15 downto 0) := "0000000000000000";
constant sixteen_one : std_logic_vector(15 downto 0) := "1111111111111111";
constant twentyfour_one : std_logic_vector(23 downto 0) := "111111111111111111111111";
constant twentyfour_zero : std_logic_vector(23 downto 0) := "000000000000000000000000";
signal mi_15 : std_logic_vector(15 downto 0);
signal mi_31 : std_logic_vector(15 downto 0);
signal mi24_7,mi24_15,mi24_23,mi24_31 : std_logic_vector(23 downto 0);

begin

WITH memory_in(31) SELECT
mi_31 <= "1111111111111111" WHEN '1',
     "0000000000000000" WHEN '0';
WITH memory_in(15) SELECT
mi_15 <= "1111111111111111" WHEN '1',
     "0000000000000000" WHEN '0';

WITH memory_in(31) SELECT
mi24_31 <= "111111111111111111111111" WHEN '1',
      "000000000000000000000000" WHEN '0';
WITH memory_in(23) SELECT
mi24_23 <= "111111111111111111111111" WHEN '1',
      "000000000000000000000000" WHEN '0';
WITH memory_in(15) SELECT
mi24_15 <= "111111111111111111111111" WHEN '1',
      "000000000000000000000000" WHEN '0';
WITH memory_in(7) SELECT
mi24_7 <= "111111111111111111111111" WHEN '1',
      "000000000000000000000000" WHEN '0';

WITH instr & byte_offset SELECT
out_val <= memory_in WHEN "000--",

     sixteen_zero & memory_in(15 downto 0) WHEN "001-0",
     sixteen_zero & memory_in(31 downto 16) WHEN "001-1",

     twentyfour_zero & memory_in(7 downto 0) WHEN "01000",
     twentyfour_zero & memory_in(15 downto 8) WHEN "01001",
     twentyfour_zero & memory_in(23 downto 16) WHEN "01010",
     twentyfour_zero & memory_in(31 downto 24) WHEN "01011",

     mi_15 & memory_in(15 downto 0) WHEN "011-0",
     mi_31 & memory_in(31 downto 16) WHEN "011-1",

     mi24_7 & memory_in(7 downto 0) WHEN "10000",
     mi24_15 & memory_in(15 downto 8) WHEN "10001",
     mi24_23 & memory_in(23 downto 16) WHEN "10010",
     mi24_31 & memory_in(31 downto 24) WHEN "10011",

     processor_in(7 downto 0)&processor_in(7 downto 0)&processor_in(7 downto 0)&processor_in(7 downto 0) WHEN "101--", 

     processor_in(15 downto 0)&processor_in(15 downto 0) WHEN "110--",

     processor_in WHEN "111--";

WITH instr SELECT
processor_out <= out_val WHEN "0--",
         out_val WHEN "100",
         sixteen_zero & sixteen_zero WHEN OTHERS;

WITH instr SELECT
memory_out <= out_val WHEN "101",
        out_val WHEN "110",
        out_val WHEN "111",
        sixteen_zero & sixteen_zero WHEN OTHERS;

WITH instr & byte_offset SELECT
memory_write_enable <= "0001" WHEN "10100",
             "0010" WHEN "10101",
             "0100" WHEN "10110",
             "1000" WHEN "10111",
             "0011" WHEN "110-0",
             "1100" WHEN "110-1",
             "0000" WHEN OTHERS;

end Behavioral;
--------------------------------------------------------------------------------------------------------------------------------




----------------------------------------------------------------Memory-------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity memory is
    Port(
    address     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    write_data  : in std_logic_vector(31 downto 0);
    write_enable,read_enable : in std_logic;
    read_data     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    clock       : in std_logic
        );
end memory; 

architecture Behavioral of memory is
type regstr is array (0 to 2047) of std_logic_vector(31 downto 0); 
signal mem : regstr;

begin
with read_enable select
read_data <= mem(to_integer(unsigned(address))) when '1',
             "00000000000000000000000000000000" when others;
process(clock)
  begin
  if (clock = '1' and clock'event) then
      if write_enable = '1' then
        mem(to_integer(unsigned(address))) <= write_data;
    end if;
  end if ;
end process;

end Behavioral;
----------------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------2-mux-4bit---------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity mux2_4bit is 
    port(
in0,in1      : in std_logic_vector(3 downto 0); 
selec        : in std_logic;
enable       : in std_logic;
out_val      : out std_logic_vector(3 downto 0)  
        );
end entity;
architecture Behavioral of mux2_4bit is
signal temp : std_logic_vector(3 downto 0);
begin
with selec select
temp <= in0 when '0',
        in1 when '1';
        
with enable select 
out_val <= temp when '1',
           "0000" when others;
end Behavioral ; -- arch
------------------------------------------------------------------------------------------------------------------------------------------



---------------------------------------------------------------2-mux-32bit---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity mux2_32bit is 
    port(
in0,in1      : in std_logic_vector(31 downto 0); 
selec        : in std_logic;
enable       : in std_logic;
out_val      : out std_logic_vector(31 downto 0)  
        );
end entity;
architecture Behavioral of mux2_32bit is
signal temp : std_logic_vector(31 downto 0);
begin
with selec select
temp <= in0 when '0',
        in1 when '1';
with enable select 
        out_val <= temp when '1',
                   "00000000000000000000000000000000" when others;
end Behavioral ; -- arch
------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------4-mux-32bit---------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity mux4_32bit is 
    port(
in0,in1,in2,in3  : in std_logic_vector(31 downto 0); 
selec            : in std_logic_vector(2 downto 0);
enable           : in std_logic;
out_val          : out std_logic_vector(31 downto 0)  
        );
end entity;
architecture Behavioral of mux4_32bit is
signal temp : std_logic_vector(31 downto 0);
begin
with selec select
temp <= in0 when "00",
        in1 when "01",
        in2 when "10",
        in3 when "11";
with enable select 
                out_val <= temp when '1',
                           "00000000000000000000000000000000" when others;
end Behavioral ; -- arch
------------------------------------------------------------------------------------------------------------------------------------------




---------------------------------------------------------------datapath----------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.STD_LOGIC_ARITH.CONV_STD_LOGIC_VECTOR;
use ieee.NUMERIC_STD.all;

entity datapath is
    port(
      instruction : out std_logic_vector(31 downto 0);
      flag        : inout std_logic_vector(3 downto 0);--cnzv
      clock,reset : in std_logic;
      I_or_D,PCinc_OR_b,Rn_OR_Rd,Rs_OR_Rm,sft_OR_mul,s,Rd_OR_lr,reg_wr_enbl,mem_wr_enbl : in std_logic;
      Reg_wrtselect : in std_logic_vector(1 downto 0);
      IR_enb,DR_enb,A_enb,B_enb,Rd_enb,shiftamt_enb,sft_or_mul_res_enb,res_enb,PC_enb,PCinc_OR_b_enb : in std_logic;
      alu_sel : in std_logic_vector(3 downto 0)      
      );
end datapath;

architecture Behavioral of datapath is
signal IR,DR,A,B,Rd,sft_or_mul_res,res,PC,shiftamt,sft_or_mul0,sft_or_mul1,wrt_data,rd_add1,rd_add2,wrt_add,mem_add : std_logic_vector(31 downto 0); 
signal IR_temp,DR_temp,A_temp,B_temp,Rd_temp,sft_or_mul_res_temp,res_temp,shiftamt_temp,PC_temp,PC_inc1,PC_inc2,bl_offset : std_logic_vector(31 downto 0);
signal flag_temp : std_logic_vector (3 downto 0);
signal sftcary : std_logic;
signal zero : std_logic_vector(31 downto 0):="00000000000000000000000000000000";
signal four : std_logic_vector(31 downto 0):="00000000000000000000000000000100";
signal junk : std_logic_vector(3 downto 0);

component ALU
Port (
    A, B     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    carry    : in std_logic;
    ALU_Sel  : in  STD_LOGIC_VECTOR(3 downto 0);  -- input 4-bit for selecting function
    ALU_out  : out  STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    flag     : out std_logic_vector(3 downto 0)        -- CNZV flags
    );
end component;


component shifter
 Port (
    A           : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    shift_type  : in std_logic_vector(1 downto 0);
    shiftamt    : in STD_LOGIC_VECTOR(4 downto 0);  -- input 5-bit for selecting function
    shifter_out     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    shift_carry : out std_logic --shifter carry
    );
end component;


component multiplier
Port (
    A,B          : in  STD_LOGIC_VECTOR(31 downto 0);  -- 2 inputs 32-bit
    mul_out      : out STD_LOGIC_VECTOR(31 downto 0) --  output 31-bit 
    );
end component;


component registr
Port(
    write_data : in std_logic_vector(31 downto 0); -- data to be written
    read_add1,read_add2 : in std_logic_vector(3 downto 0); -- 4-bit read address for read ports
    write_add  : in std_logic_vector(3 downto 0); -- 4-bit address for writing
    clock, reset, write_enable : in std_logic;
    read_data1,read_data2  : out std_logic_vector(31 downto 0);
    PC : inout std_logic_vector(31 downto 0)
  ); 
end component;


component memory
Port(
    address     : in  STD_LOGIC_VECTOR(31 downto 0);  -- 1 inputs 32-bit
    write_data  : in std_logic_vector(31 downto 0);
    write_enable,read_enable : in std_logic;
    read_data     : out STD_LOGIC_VECTOR(31 downto 0); --  output 31-bit 
    clock         : in std_logic 
    );
end component;


component mux2_4bit
port(
in0,in1      : in std_logic_vector(3 downto 0); 
selec        : in std_logic;
enable       : in std_logic;
out_val      : out std_logic_vector(3 downto 0)  
        );
end component;

component mux2_32bit
port(
in0,in1      : in std_logic_vector(31 downto 0); 
selec        : in std_logic;
enable       : in std_logic;
out_val      : out std_logic_vector(31 downto 0)  
        );
end component;

component mux4_32bit
port(
in0,in1,in2,in3  : in std_logic_vector(31 downto 0); 
selec            : in std_logic_vector(2 downto 0);
enable           : in std_logic;
out_val          : out std_logic_vector(31 downto 0)  
        );
end component;


begin

main_ALU: ALU port map(A,sft_or_mul_res,'0',alu_sel,res_temp,flag_temp);
with res_enb select
res <= res_temp when '1',
       res when others;

with s select
flag <= flag_temp when '1',
       flag when others;

shift : shifter port map(B,IR(6 downto 5),shiftamt(4 downto 0),sft_or_mul0,sftcary);

multiply : multiplier port map(B,shiftamt,sft_or_mul1);

sftORmul : mux2_32bit port map(sft_or_mul0,sft_or_mul1,sft_OR_mul,'1',sft_or_mul_res_temp );


with sft_or_mul_res_enb select
sft_or_mul_res <= sft_or_mul_res_temp when '1',
                  sft_or_mul_res when others;

reg_rdadd_sel_1 : mux2_4bit port map(IR(15 downto 12),IR(19 downto 16),Rn_OR_Rd,'1',rd_add1);
reg_rdadd_sel_2 : mux2_4bit port map(IR(3 downto 0),IR(11 downto 8),Rs_OR_Rm,'1',rd_add2);
reg_wradd_sel   : mux2_4bit port map(IR(15 downto 12),"1110",Rd_OR_lr,'1',wrt_add);

reg_wr_sel : mux4_32bit port map(DR,res,PC,zero,Reg_wrtselect,'1',wrt_data);

Rgstr : register port map(wrt_data,rd_add1,rd_add2,wrt_add,clock,reset,reg_wr_enbl,A_temp,B_temp,PC);
A_temp <= Rd_temp;
B_temp <= shiftamt_temp;

with A_enb select
A <= A_temp when '1',
     A  when others;

with B_enb select
B <= B_temp when '1',
     B  when others;

with Rd_enb select
Rd <= Rd_temp when '1',
      Rd when others;

with shiftamt_enb select
shiftamt <= shiftamt_temp when '1',
            shiftamt when others;

mem_add_sel : mux2_32bit port map(PC,res,I_or_D,'1',mem_add);

mem : memory port map(mem_add,Rd,mem_wr_enbl,'1',IR_temp,clock);

DR_temp <= IR_temp;

with IR_enb select
IR <= IR_temp when '1',
      IR when others;

with DR_enb select
DR <= DR_temp when '1',
      DR when others;

bl_offset <= four + ("000000" && ins(23 downto 0) && "00");

PC_inc : ALU port map(PC,four,'0',four(3 downto 0),PC_inc1,junk);

PC_inc_bl : ALU port map(PC,bl_offset,'0',four(3 downto 0),PC_inc2,junk);

PC_inc_sel : mux2_32bit port map(PC_inc1,PC_inc2,PCinc_OR_b,PCinc_OR_b_enb,PC_temp);

with mem_wr_enbl select
PC <= PC_temp when '1',
      PC when others; 


end Behavioral;

-----------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------16bitmux----------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity mux16 is
    Port(
in_val : in std_logic_vector(15 downto 0);
selec : in std_logic_vector(3 downto 0);
out_val :out std_logic
        );
end entity;

architecture Behavioral of mux16 is
begin
out_val <= inval(to_integer(unsigned(selec)));
end Behavioral;
--------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------Controller---------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.NUMERIC_STD.all;

entity controller is
port(
      instruction : in std_logic_vector(31 downto 0);
      flag        : in std_logic_vector(3 downto 0);--cnzv 
      clock,reset : in std_logic;
      I_or_D,PCinc_OR_b,Rn_OR_Rd,Rs_OR_Rm,sft_OR_mul,s,Rd_OR_lr,reg_wr_enbl,mem_wr_enbl : out std_logic;
      Reg_wrtselect : out std_logic_vector(1 downto 0);
      IR_enb,DR_enb,A_enb,B_enb,Rd_enb,shiftamt_enb,sft_or_mul_res_enb,res_enb,PC_enb,PCinc_OR_b_enb : out std_logic;
      alu_sel : out std_logic_vector(3 downto 0)      
      );
end entity;


architecture Behavioral of controller is
signal in_val_temp : std_logic_vector(15 downto 0);
signal p : std_logic;

component mux16
Port(
in_val : in std_logic_vector(15 downto 0);
selec : in std_logic_vector(3 downto 0);
out_val :out std_logic
        );
end component;

begin

in_val_temp(0) <= flag(1);
in_val_temp(1) <= Not flag(1);
in_val_temp(2) <= flag(3);
in_val_temp(3) <= Not flag(3);
in_val_temp(4) <= flag(2);
in_val_temp(5) <= Not flag(2);
in_val_temp(6) <= flag(0);
in_val_temp(7) <= NOt flag(0);
in_val_temp(8) <= flag(3) and (Not flag(1));
in_val_temp(9) <= (Not flag(3)) and flag(1);
in_val_temp(10) <= Not (flag(2) xor flag(0));
in_val_temp(11) <= flag(2) xor flag(0);
in_val_temp(12) <= (Not (flag(2) xor flag(0))) and (Not flag(1));
in_val_temp(13) <= (flag(2) xor flag(0)) and flag(1);
in_val_temp(14) <= '1';

mux_16 : mux16 port map(in_val_temp,instruction(31 downto 28),p)

end Behavioral;
