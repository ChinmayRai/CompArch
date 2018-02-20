library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mp_datapath is
	Port(
		memory_in,processor_in : in std_logic_vector(31 downto 0); 
		instr: in std_logic_vector(2 downto 0); 
		byte_offset : in std_logic_vector(1 downto 0);
		memory_write_enable : out std_logic_vector(3 downto 0);
		memory_out,processor_out : out std_logic_vector(31 downto 0)
	); 
end mp_datapath;

--000 : ldr
--001 : ldrh
--010 : ldrb
--011 : ldrsh
--100 : ldrsb
--101 : strb
--110 : strh
--111 : str

architecture Behavioral of mp_datapath is
signal out_val : std_logic_vector(31 downto 0);
constant sixteen_zero : std_logic_vector(15 downto 0) := "0000000000000000";
constant sixteen_one : std_logic_vector(15 downto 0) := "1111111111111111";
constant twentyfour_one : std_logic_vector(23 downto 0) := "111111111111111111111111";
constant twentyfour_zero : std_logic_vector(23 downto 0) := "000000000000000000000000";
signal mi_15 : std_logic_vector(15 downto 0);
signal mi_31 : std_logic_vector(15 downto 0);
signal cool7,cool15,cool23,cool31 : std_logic_vector(23 downto 0);

begin

WITH memory_in(31) SELECT
mi_31 <= "1111111111111111" WHEN '1',
		 "0000000000000000" WHEN '0';
WITH memory_in(15) SELECT
mi_15 <= "1111111111111111" WHEN '1',
		 "0000000000000000" WHEN '0';

WITH memory_in(31) SELECT
cool31 <= "111111111111111111111111" WHEN '1',
		  "000000000000000000000000" WHEN '0';
WITH memory_in(23) SELECT
cool23 <= "111111111111111111111111" WHEN '1',
		  "000000000000000000000000" WHEN '0';
WITH memory_in(15) SELECT
cool15 <= "111111111111111111111111" WHEN '1',
		  "000000000000000000000000" WHEN '0';
WITH memory_in(7) SELECT
cool7 <= "111111111111111111111111" WHEN '1',
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

		 cool7 & memory_in(7 downto 0) WHEN "10000",
		 cool15 & memory_in(15 downto 8) WHEN "10001",
		 cool23 & memory_in(23 downto 16) WHEN "10010",
		 cool31 & memory_in(31 downto 24) WHEN "10011",

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
