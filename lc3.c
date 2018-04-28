/* 
 * CS 350 - Hale
 * Final Project: LC-3 Simulator 
 *
 * Illinois Institute of Technology, (c) 2017, Kyle C. Hale
 *
 * This is the skeleton code for the final project (LC-3 simulator). 
 *
 */

#include <stdio.h>
#include <stdlib.h>       // For error exit()
#include <string.h>       // for strcmp
#include "lc3.h"


/*
 * Explains the usage of this program
 */
static void
usage (char *argv[]) {
	printf("Usage: %s <.hex file>\n", argv[0]);
	exit(0);
}




static word_t
hex_code_to_short(const char* str) {
	int i = 0;
	word_t value = strtol(str, NULL, 16) & 0xFFFF;
	return value;
}






static int
read_hex_number(FILE *datafile, unsigned int *value_read) {

#define HEX_BUFFER_LEN 180
	char hex_buffer[HEX_BUFFER_LEN];   // Text of current line
	int words_read;                    // Nbr words read f/buffer
	char *read_success;                // NULL if reading in a line fails
	int found_nbr = 0;                 // true if we have read a hex nbr

	// Read a line from the data file and read a hex value from
	// the line.  Return true (1) if we succeed; return false (0) if the
	// data file read or hex value read failed.
	// Note if the line of input begins with an integer, treat
	// it as the memory value to read in.  Ignore junk
	// after the number and ignore blank lines and lines
	// that don't begin with a number.
	do {
		read_success = fgets(hex_buffer, HEX_BUFFER_LEN, datafile);
		words_read = sscanf(hex_buffer, "%x", value_read);
	} while (words_read != 1);

	return !(read_success == NULL);
}



/* -------------------- CONDITION CODE ROUTINES --------------------
 *
 * Return character N, Z, or P depending on cpu condition code.
 * Return '?' for a bad condition code.
 */

static char
cc2char (cpu_t *cpu) {
	cc_t cc = cpu->cc;
	if (cc == CC_NEG) {
		return 'N';
	} else if (cc == CC_ZER) {
		return 'Z';
	} else if (cc == CC_POS) {
		return 'P';
	} else {
		return '?';
	}
}


/* 
 * Set cc to 4 if value is negative, 2 if zero, 1 if positive.
 *
 */
static void
set_cc (cpu_t *cpu, word_t value, instr_t *instr) {


	// set the condition codes based on the value
	// passed in. See the definitions of CC_NEG, etc. in lc3.h
	if (value < 0)  cpu->cc = CC_NEG;
	if (value == 0) cpu->cc = CC_ZER;
	if (value > 0)  cpu->cc = CC_POS;

	printf("CC = %c", cc2char(cpu));

	// Add trailing newline except for trap instructions
	if (instr->opcode != 0xF) {
		printf("\n");
	}
}


/* 
 * field(value, hi, lo) returns bits value[hi:lo], zero-filled.
 * (Bits are numbered 15..0, left to right.)
 */
static word_t
field (word_t value, int high, int low) {
	// Say we want to select n = high-low+1 bits.
	// Then (value >> low) right-justifies those n bits,
	// and (0xffff << n) is a mask with 16-n 1's and n 0's
	// so ~(0xffff << n) is a mask with 16-n 0's and n 1's.
	//
	//return (value >> low) & ~((Word) 0xffff << (high-low+1));
	word_t mask = (1 << (high-low+1)) - 1;
	return (value >> low) & mask;
}



/* 
 * sign_ext(value, hi, lo) returns bits value[hi:lo], sign-filled.
 * (Bits are numbered 15..0, left to right.)
 *
 */
static word_t
sign_ext(word_t value, int high, int low) {

	// Take the index 'high' as the bit
	// position that contains the sign bit. Drag this
	// bit across appropriately to sign extend the number
	// to a full 16 bits.
	int left_shift = 15 - high;
	int right_shift = low + left_shift;
	word_t s_value = value << left_shift;
	s_value = s_value >> right_shift;

	return s_value;
}


/* -------------------- PRINT INSTRUCTION ROUTINES -------------------- */




/* 
 * Format for printing instructions
 *
 * Opcodes are 5 chars, left-justified; there's a space and then a sequence
 * of registers and/or values, separated by comma space. Registers are R0 -- R7.
 * Decimal values are left-justified with a leading hyphen (for negative values)
 *     or a leading space (for non-negative values)
 * Hex values are printed as 2 hex digits with preceding x
 * Trailing spaces are printed out so that every instruction takes 18 characters.

 * print_op(op), print_val(op, val) and print_hexval(op, val) 
 * prints the opcode and (if given) the value in decimal or hex.
 */
static void
print_op (char *op) {
	printf("%-5s %13s", op, "");
}		

static void
print_val (char *op, word_t val) {
	printf("%-5s %-6d %6s", op, val, "");
}

static void
print_hexval (char *op, word_t val) {
	printf("%-5s x%-2hX %9s", op, val, ""); 
}

/* 
 * print_reg_val(op, reg, val) and print_2reg_val(op, r1, r2, val)
 * prints out the opcode, register(s), and value
 */
static void
print_reg_val (char *op, reg_t reg, word_t val) {
	printf("%-5s R%d, %-5d %3s", op, reg, val,"");
}

static void
print_2reg_val (char *op, reg_t r1, reg_t r2, word_t val) {
	printf("%-5s R%d, R%d, %-4d ", op, r1, r2, val);
}

/* 
 * print_reg(op, reg), print_2reg(op, r1, r2), and print_3reg(op, r1, r2, r3)
 * prints out the opcode and register(s)
 */
static void
print_reg (char *op, reg_t reg) {
	printf("%-5s R%d %10s", op, reg, "");
}

static void
print_2reg (char *op, reg_t r1, reg_t r2) {
	printf("%-5s R%d, R%d %6s", op, r1, r2, "");
}

static void
print_3reg (char *op, reg_t r1, reg_t r2, reg_t r3) {
	printf("%-5s R%d, R%d, R%d %2s", op, r1, r2, r3, "");
}


/* 
 * print_instr(instr) prints a word as an assembler instruction, with a
 * mnemonic and some number of register or value arguments.
 * An 18-char string is printed (with no trailing '\n').
 */
void 
print_instr (instr_t *instr) {
	char *op_mnemonic[16] = {"BR","ADD","LD","ST","JSR","AND","LDR","STR",
		"RTI","NOT","LDI","STI","JMP","ERR","LEA","TRAP"};
	int op_field = instr->opcode;
	char *op = op_mnemonic[op_field];
	char *br_mnemonic[8] = {"NOP","BRP","BRZ","BRZP","BRN","BRNP","BRNZ","BR"};

	switch (op_field) {
		case 0: // BR 
			{
				br_t * b = (br_t*)instr;
				int cc_field = (b->val >> 9) & 0x7;
				print_val(br_mnemonic[cc_field], sign_ext(instr->val,8,0));
				break;
			}
		case 1: 
		case 5: // ADD, AND
			{
				add_and_reg_t* a = (add_and_reg_t*)instr;

				if (a->flag == 0) { // 0 means reg
					print_3reg(op, a->dr, a->sr1, a->sr2);
				} else { // 1 means immediate
					add_and_imm_t * addi = (add_and_imm_t*)instr;
					print_2reg_val(op, addi->dr, addi->sr1, sign_ext(addi->val,4,0));
				}
				break;
			}
		case 2: 
		case 3: 
		case 10: 
		case 11: 
		case 14: // LD, ST, LDI, STI, LEA
			{
				ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
				print_reg_val(op, l->dr, sign_ext(l->val, 8, 0));
				break;
			}
		case 4: // JSR/JSRR
			{
				jsr_t * jsr = (jsr_t*)instr;

				if (jsr->flag == 0) {
					jsrr_t * jsrr = (jsrr_t*)instr;
					print_reg("JSRR", jsrr->baser);
				} else { 
					print_val("JSR", sign_ext(jsr->val, 10, 0));
				}
				break;
			}
		case 6: 
		case 7: // LDR, STR
			{
				ldr_t * l = (ldr_t*)instr;
				print_2reg_val(op, l->dr, l->baser, sign_ext(l->val, 5, 0));
				break;
			}
		case 8: 
		case 13: // RTI, ERR
			print_op(op);
			break;
		case 9: // NOT
			{
				not_t * n = (not_t*)instr;
				print_2reg(op, n->dr, n->sr);
				break;
			}
		case 12: // JMP
			{
				jmp_ret_t * j = (jmp_ret_t*)instr;
				print_reg(op, j->baser);
				break;
			}
		case 15: // TRAP
			{
				trap_t *t = (trap_t*)instr;
				print_hexval(op, t->trapvect8);
				break;
			}
		default: 
			break;
	}
}


/* 
 * print_addr_val_hex_instr(addr, val) prints
 * out the address in hex colon, value in decimal,
 * and value as an instruction.
 */
static void
print_addr_val_hex_instr (address_t addr, instr_t *instr) {
	printf("x%04hX: x%04hX  ", addr, instr->val);
	print_instr(instr);
}


/* 
 * print_addr_val_hex_dec_instr(addr, val) prints
 * out the address in hex colon, value in hex, value in decimal,
 * and value as an instruction.
 */
static void
print_addr_val_hex_dec_instr (address_t addr, instr_t *instr) {
	if (instr->val != 0) {
		printf("x%04hX: x%04hX  % 6d  ", addr, instr->val, instr->val);
		print_instr(instr);
		printf("\n");
	}
}




/* -------------------- SIMULATOR ROUTINES -------------------- */


/* 
 * Print standard message for simulator help command ('h' or '?')
 */
static void
help_msg (void) {
	printf("Simulator commands:\n");
	printf("h, H, or ? for help (prints this message)\n");
	printf("q or Q to quit\n");
	printf("d or D to dump the control unit and memory\n");
	printf("An integer > 0 to execute that many instruction cycles\n");
	printf("Or just return, which executes one instruction cycle\n");
}


/*
 * This function will look at the IR and will fill in the
 * values of an instruction struct
 */
static void
decode_instr (word_t ir, instr_t *instr) {
	// Note that we don't really need to do any decoding here as
	// that will be taken care of by casting the instruction 
	// to different types of structs (see lc3.h and explanations)
	instr->val = ir;
}


/* -------------------- LC-3 INSTRUCTION ROUTINES --------------------
 *
 * Execute branch instruction: Bitwise AND instruction's mask and
 * cpu's condition code, branch iff result is nonzero.
 *
 * Echo kind of branch, current cc, whether or not the branch happened,
 * and if so, the target of the branch.
 *
 * NOTE: branch does NOT modify condition codes
 *
 */
static void
instr_br (cpu_t *cpu, instr_t *instr) {
	br_t * b = (br_t*)instr;

	// extract cc from the decoded branch instruction,
	// it should not just be 0!
	uint8_t cc = (b->n << 2) + (b->z << 1) + (b->p);
	// br_t br = (br_t)*instr;
	// printf("BRANCH %c %c %c\n", br.p, br.z, br.n);
	printf("; CC = %c, ", cc2char(cpu));

	if ((cc & cpu->cc) == 0) {
		printf("no branch\n");
	} else {
		address_t target = (cpu->pc + b->pcoffset9);
		printf("PC <- x%hX%+d = x%hX\n", cpu->pc, b->pcoffset9, target);
		cpu->pc = target;
	}
}


/* 
 * Execute add instruction: Destination register = left operand + right
 * operand.  Left operand is src1 register, right operand is either src2
 * register or an immediate field (sign-filled).
 *
 * Echo type of add, sources/values of operand, destination/value of
 * result.;
 *
 * NOTE: add DOES modify condition codes:
 *
 */
static void
instr_add (cpu_t *cpu, instr_t *instr) {

	add_and_reg_t *add_instr     = (add_and_reg_t*)instr;
	add_and_imm_t *add_instr_imm = (add_and_imm_t*)instr;

	// finish this function, setting the result, and
	// setting the condition codes properly. Note that this should
	// handle both add and add immediate. Note the check for the flag
	// in the decoded instruction to see which one it is. You may need
	// to cast instr as a add_and_imm_t*! You'll have to change the 
	// arguments to the printfs!

	word_t add_result = 0;

	if (add_instr->flag == 1) {
		// Immediate mode
		add_and_imm_t *im = (add_and_imm_t*)instr; 
		word_t a = cpu->reg[im->sr1];
		word_t b = sign_ext(im->val, 4, 0);
		add_result = a + b;
		printf("; R%d <- x%hX %+d = x%hX; ", im->dr, (short) a, b, (short)add_result);
	} else {
		add_and_reg_t *im = (add_and_reg_t*)instr; 
		word_t a = cpu->reg[im->sr1];
		word_t b = cpu->reg[im->sr2];
		add_result = a + b;
		printf("; R%d <- x%hX + x%hX = x%hX; ", im->dr, (short)a, (short)b, (short)add_result);
	}

	cpu->reg[add_instr->dr] = add_result;
	set_cc(cpu, add_result, instr);
}


/* 
 * Execute load instruction: Load destination register
 * from PC-offset memory location.  Set condition code.
 *
 * Echo destination, pc, offset, pc+offset, value loaded,
 * new condition code.
 *
 * NOTE: ld DOES modify condition codes
 * 
 */
static void
instr_ld (cpu_t *cpu, instr_t *instr) {

	ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
	uint8_t dr = l->dr;
	int16_t offset = l->pcoffset9;
	address_t location = cpu->pc + offset;
	word_t value = cpu->mem[location];
	cpu->reg[dr] = value;
	printf("; R%d <- M[x%04hX] = x%hX; ", dr, location, value);
	set_cc(cpu, value, instr);
}


/* 
 * Execute store instruction: Store source register
 * to PC-offset memory location.  Set condition code.
 *
 * Echo source, pc, offset, pc+offset, value stored,
 * new condition code.
 *
 * NOTE: ST does not modify condition codes
 *
 */
static void
instr_st (cpu_t *cpu, instr_t *instr) {
	st_sti_t *s = (st_sti_t*)instr;

	address_t addr   = cpu->pc + s->pcoffset9;
	word_t    value  = cpu->reg[s->sr];
	

	printf("; M[x%04hX] <- x%hX\n", (short)addr, (short)value);
	cpu->mem[addr] = value;
}


/* 
 * Execute jump subroutine instruction: Save R7 and branch
 * to target location (PC-offset or base register).
 * 
 * NOTE: This handles both JSR and JSRR!
 *
 * Echo kind of JSR, target, and either PC & offset or
 * base register number.
 *
 * NOTE: jsr does NOT modify condition codes
 *
 *
 */
static void
instr_jsr (cpu_t *cpu, instr_t *instr) {
	jsr_t *j = (jsr_t*)instr;
	address_t old_pc = cpu->pc;
	cpu->reg[7] = cpu->pc;
	
	if (j->flag == 1) { // this is JSR
		address_t dest = cpu->pc + j->pcoffset11;
		cpu->pc = dest;
		printf("; PC <- x%hX %+d = x%hX, R7 <- x%hX\n", (short)old_pc, j->pcoffset11, (short)dest, (short)old_pc);
	} else { // this is JSRR
		jsrr_t *jsrr = (jsrr_t*)instr;
		cpu->pc = cpu->reg[jsrr->baser];
		printf("; PC <- x%hX, R7 <- x%hX\n", (short)cpu->pc, (short)old_pc);
	}
}


/* 
 * Execute and instruction: Destination register = left operand & right
 * operand.  Left operand is src1 register, right operand is either src2
 * register or an immediate field (sign-filled).  Set condition code.
 *
 * Echo type of add, sources/values of operand, destination/value of
 * result, new condition code.
 *
 * NOTE: and DOES modify condition codes
 *
 */
static void
instr_and (cpu_t *cpu, instr_t *instr) {
	add_and_reg_t *a = (add_and_reg_t*)instr;
	
	word_t value = 0;
	word_t v1, v2 = 0;
	
	if (a->flag == 1) {
		add_and_imm_t *i = (add_and_imm_t*)instr;
		v1 = cpu->reg[i->sr1];
		v2 = i->imm5;
		value = v1 & v2;
		printf("; R%d <- x%04hX & x%04hX = x%hX; ", i->dr, (short)v1, (short)v2, (short)value);
	} else {
		v1 = cpu->reg[a->sr1];
		v2 = cpu->reg[a->sr2];
		value = v1 & v2;
		printf("; x%04hX & x%04hX = x%hX; ", (short)v1, (short)v2, (short)value);
	}
	
	cpu->reg[a->dr] = value;
	set_cc(cpu, value, instr);
}


/* 
 * Execute load using base register: Load destination register
 * from base-offset memory location.  Set condition code.
 *
 * Echo destination, base, offset, base+offset, value loaded,
 * new condition code.
 * 
 * NOTE: ldr DOES modify condition codes
 *
 */
static void
instr_ldr (cpu_t *cpu, instr_t *instr) {
	ldr_t *l = (ldr_t*)instr;


	address_t base = cpu->reg[l->baser];
	address_t loc = base + l->offset6;
	word_t val = cpu->mem[loc];
	
	cpu->reg[l->dr] = val;
	printf("; R%d <- M[x%04hX %+d] = M[x%04hX] = x%hX; ",
			l->dr, base, l->offset6, loc, val);
	
	set_cc(cpu, val, instr);
}


/* 
 * Execute store using base register: Store source register
 * to base-offset memory location.
 *
 * Echo source, base, offset, base+offset, value stored.
 * 
 * NOTE: str does NOT modify condition codes
 *
 */
static void
instr_str (cpu_t *cpu, instr_t *instr) {
	str_t *s = (str_t*)instr;
	word_t value = cpu->reg[s->sr];
	word_t base = cpu->reg[s->baser];

	word_t offset = s->offset6;
	address_t dest = base + offset;

	printf("; M[x%04hX %+d] = M[x%04hX] <- x%hX\n",
			base, offset, dest, value);

	cpu->mem[dest] = value;

}


/* 
 * Return from interrupt command prints a message but continues execution.
 * THIS OPCODE IS IGNORED FOR OUR PURPOSES.
 */
static void
instr_rti (cpu_t *cpu, instr_t *instr) {
	printf("; Opcode ignored\n");
}


/* 
 * Execute not instruction: Destination register = bitwise not
 * of source register.  Set condition code.
 *
 * Echo destination and source registers and values, result value,
 * new condition code.
 *
 * NOTE: not DOES modify condition codes
 *
 */
static void
instr_not (cpu_t *cpu, instr_t *instr) {
	not_t *n = (not_t*)instr;

	word_t value = cpu->reg[n->sr];

	printf("; R%d <- NOT x%04hX = x%04hX; ", n->dr, (short)value, (short)~value);
	value = ~value;

	cpu->reg[n->dr] = value;

	set_cc(cpu, value, instr);
}


/* 
 * Execute load indirect: Load destination register from
 * indirect pc+offset memory location.  Set condition code.
 *
 * Echo destination, base, offset, base+offset, value at base+offset,
 * value loaded, new condition code.
 *
 * NOTE: ldi DOES modify condition codes
 *
 *
 */
static void
instr_ldi (cpu_t *cpu, instr_t *instr) {
	ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;
	address_t memloc = cpu->pc + l->pcoffset9;
	address_t memloc2 = cpu->mem[memloc];
	word_t value = cpu->mem[memloc2];
	printf("; R%d <- M[M[x%04hX]] = M[x%04hX]  = x%hX; ", 
			l->dr, (short)memloc, (short)memloc2, value);

	cpu->reg[l->dr] = value;
	set_cc(cpu, value, instr);
}


/* 
 * Execute store indirect: Store source register to indirect
 * pc+offset memory location.
 *
 * Echo source, base, offset, base+offset, value at base+offset,
 * value stored.
 *
 * NOTE: sti does NOT modify condition codes
 *
 */
static void
instr_sti (cpu_t *cpu, instr_t *instr) {
	st_sti_t *s = (st_sti_t*)instr;
	word_t sourceval = cpu->reg[s->sr];
	address_t loc1 = s->pcoffset9 + cpu->pc;
	address_t loc2 = cpu->mem[loc1];
	printf("; M[M[x%04hX]]= M[x%04hX] <- x%hX\n", (short)loc1, (short)loc2, (short)sourceval);
	cpu->mem[loc2] = sourceval;
}


/* 
 * Execute jump instruction: Set PC to base register.
 *
 * Echo base register and value.
 *
 * NOTE: jmp does NOT modify condition codes
 *
 */
static void
instr_jmp (cpu_t *cpu, instr_t *instr) {
	jmp_ret_t *j = (jmp_ret_t*)instr;

	address_t dest = cpu->reg[j->baser];

	printf("; PC <- x%hX\n", (short)dest);

	cpu->pc = dest;
}


/* 
 * Reserved opcode causes prints message but continues execution.
 */
static void
instr_err (cpu_t *cpu, instr_t *instr) {
	printf("; Reserved opcode; ignored.\n");
}


/* 
 * Execute load effective address: Load destination register
 * with base-offset memory location.  Set condition code.
 *
 * Echo destination, base, offset, base+offset, new condition code.
 *
 * NOTE: lea DOES modify condition codes
 *
 *
 */
static void
instr_lea (cpu_t *cpu, instr_t *instr) {
	ld_ldi_lea_t *l = (ld_ldi_lea_t*)instr;

	address_t value = cpu->pc + l->pcoffset9;

	printf("; R%d <- x%hX; ", l->dr, (short)value);
	cpu->reg[l->dr] = value;
	set_cc(cpu, value, instr);
}


/* 
 * Read and return a character from standard input.  User must
 * enter return after the char.  Just pressing return causes '\n'
 * to be returned.  Any extra characters after the first are ignored.
 */
static char 
read_char (void) {
	char buffer[3] = "";
	fgets(buffer, sizeof(buffer), stdin);
	return buffer[0];
}


static int 
char_part (word_t w) {
	return sign_ext(w, 7, 0);
}


/* 
 * Execute trap instruction according to trap vector.  (Set R7
 * to return location first.)
 *
 * TRAP x20 (GETC) Read char from stdin to R0[7:0].
 * TRAP x23 (IN)   Like GETC but prompt first and echo the char.
 * TRAP x21 (OUT)  Print char (whose ASCII repr. is) in R0[7:0]
 * TRAP x22 (PUTS) Print string starting at M[R0], stop at \0.
 * TRAP x25 (HALT) Halt execution (set CPU running flag to false).
 * Bad trap vectors cause an error message and halt.
 *
 * Echo vector in all cases.  Echo char read in for GETC, IN,
 * echo char printed for OUT.
 *
 * NOTE: SOME of these trap variants modify condition codes
 *
 * FILL ME IN
 *
 */
static void
instr_trap (cpu_t *cpu, instr_t *instr) {
	trap_t *t = (trap_t*)instr;
	cpu->reg[7] = (word_t)(cpu->pc); // save ret addr

	char ch;	                // character read by GETC or IN
	word_t left_mask  = 0xff00;   // To select left byte of a Word
	word_t right_mask = 0x00ff;   // To select right byte of a Word

	printf("; ");

	switch (t->trapvect8) {
		case 0x20: 
		case 0x23: // GETC, IN: Set R0 <- read-in char

			set_cc(cpu, cpu->reg[7], instr);

			if (t->trapvect8 == 0x20) {
				printf("; GETC: ");
			} else {
				printf("; IN: Input a character>");
			}

			ch = read_char();
			

			// Only set the right half of R0 with the char
			cpu->reg[0] = (left_mask & cpu->reg[0]) | (right_mask & ch);
			printf("Read %c = %d\n", (char) ch, ch);
			break;

		case 0x21: // OUT: Print char in (right byte of) R0

			// set ch appropriately (using char_part() utility 
			// function). Also set the condition code appropriately.
			set_cc(cpu, cpu->reg[7], instr);
			ch = char_part(cpu->reg[0]);
			printf("; OUT: %d = %c\n", ch, (char) ch);
			break;

		case 0x22: // PUTS: Print string at R0

			set_cc(cpu, cpu->reg[7], instr); // set CC by return addr
			printf("; PUTS: ");
			address_t loc = cpu->reg[0];
			ch = (char) char_part(cpu->mem[loc]);
			// FILL ME IN: This might error, check here:
			while (cpu->mem[loc]) {
				putchar(cpu->mem[loc++]);
			}
			printf("\n");
			break;

		case 0x25: // HALT execution
			// should set running flag properly
			// and set condition code to positive for halt
			cpu->running = 0;
			cpu->cc = CC_POS;
			printf("CC = P; Halting\n");
			break;

		default:
			printf("; Bad trap vector (halting)\n");
			cpu->running = 0;
			return; 
	}
}


/* -------------------- INSTRUCTION CYCLE ROUTINES -------------------- */


/* 
 * Execute one instruction cycle
 */
static void
one_instruction_cycle (cpu_t *cpu) {

		// test if the cpu is running. If it isn't, print
	// "Halted\n" and return.
	if (cpu->running == 0) {
		printf("Halted\n");
		return;
	}

	address_t instr_loc = cpu->pc;  // Instruction's addr (pc before increment)

	// load an instruction into IR and increment PC
	cpu->ir = cpu->mem[cpu->pc++];

	// DECODE
	instr_t instr;
	decode_instr(cpu->ir, &instr);

	// Echo instruction
	print_addr_val_hex_instr(instr_loc, &instr);

	switch (instr.opcode) {
		case  0:
			instr_br(cpu, &instr);
			break;
		case  1:
			instr_add(cpu, &instr);
			break;	
		case  2:
			instr_ld(cpu, &instr);
			break;
		case  3:
			instr_st(cpu, &instr);
			break;
		case  4:
			instr_jsr(cpu, &instr);
			break;
		case  5:
			instr_and(cpu, &instr);
			break;	
		case  6: 
			instr_ldr(cpu, &instr);
			break;
		case  7:
			instr_str(cpu, &instr);
			break;
		case  8:
			instr_rti(cpu, &instr);
			break;
		case  9:
			instr_not(cpu, &instr);
			break;
		case 10:
			instr_ldi(cpu, &instr);
			break;
		case 11:
			instr_sti(cpu, &instr);
			break;
		case 12:
			instr_jmp(cpu, &instr);
			break;	
		case 13: 
			instr_err(cpu, &instr);
			break;
		case 14:
			instr_lea(cpu, &instr);
			break; 
		case 15: 
			instr_trap(cpu, &instr);
			break;
		default:
			{
				printf("Bad opcode: %d; quitting\n", instr.opcode);
				cpu->running = 0;
				break;
			}
	}
}

/*
 * Execute a number of instruction cycles.  Exceptions: If the
 * number of cycles is <= 0, complain and return; if the CPU is
 * not running, say so and return; if the number of cycles is
 * too big, substitute a saner limit.
 *
 * If, as we execute the many cycles, the CPU stops running,
 * then return.
 */
static void
many_instruction_cycles (int nbr_cycles, cpu_t *cpu) {
	if (nbr_cycles < 1) {
		printf("Number of instruction cycles to do should be > 0\n");
		return;
	} else if (!cpu -> running) {
		printf("Halted\n");
		return;
	}

	if (nbr_cycles > MULTI_INSTR_LIMIT) {
		nbr_cycles = MULTI_INSTR_LIMIT;
	}

	int cycle;
	for (cycle = 0; cpu->running && cycle < nbr_cycles; cycle++) {
		one_instruction_cycle(cpu);
	}
}


/* 
 * Execute a nonnumeric command; complain if it's not ? h d j m q r
 * Return true for the q command, false otherwise
 */
static int 
exec_cmd (char cmd_char, char *command, cpu_t *cpu) {

	switch (cmd_char) {
		case '?':
		case 'h':
		case 'H':
			{
				help_msg();
				break;
			}
		case 'd':
		case 'D':
			{
				dump_cpu(cpu);
				dump_memory(cpu, 0, 0xffff);
				break;
			}
		case 'q':
		case 'Q':
			{
				printf("Quitting\n");
				return 1;
			}
		case '\n': 
			{
				one_instruction_cycle(cpu);
				break;
			}
		default: 
			{
				printf("There is no %c command.\n", cmd_char);
				break;
			}
	}

	return 0;
}


/* 
 * Read a simulator command from the keyboard ("h", "?", "d", number,
 * or empty line) and execute it.  Return true if we hit end-of-input
 * or execute_command told us to quit.  Otherwise return false.
 *
 * finish this
 */

static int 
read_exec_cmd (cpu_t *cpu) {
	int nbr_cycles = 1;           // nbr of instr cycles to do 
	char cmd_char;                // command if not number
	int done = 0;                 // Should simulator stop?

#define CMD_BUFFER_LEN 80
	char cmd_buffer[CMD_BUFFER_LEN];  // Text of current line
	char *read_success;               // NULL if reading in a line fails.

	/*
	 * Read in a command line using fgets; if there wasn't one, we're done
	 * else use sscanf on the command line buffer to read an integer
	 * number of cycles. If the sscanf succeeds, execute that many 
	 * instruction cycles (by calling many_instruction_cycles appropriately).
	 * Else use sscanf on the command line buffer
	 * to read in a character cmd_char ('q', 'h', '?', 'd', or '\n')
	 * and call exec_cmd on cmd_char
	 *
	 * HINT: you can probably recycle your lab6 solution here...
	 */
	printf("$> ");

	if (fgets(cmd_buffer, CMD_BUFFER_LEN, stdin)) {
		if (cmd_buffer[0] == '\n') {
			one_instruction_cycle(cpu);
		} else if (sscanf(cmd_buffer, "%i", &nbr_cycles)) {
			many_instruction_cycles(nbr_cycles, cpu);
		} else if (sscanf(cmd_buffer, "%c", &cmd_char)) {
			done = exec_cmd(cmd_char, cmd_buffer, cpu);
		}
	} else {
		done = 1;
	}
	// fgets(cmd_buffer, CMD_BUFFER_LEN, stdin);
	// if (0 && strlen(cmd_buffer) == 0) {
	// 	done = 1;
	// 	return done;
	// }

	// // Read the first char from the char buffer;
	// cmd_char = cmd_buffer[0];
	// if (sscanf(cmd_buffer, "%d", nbr_cycles)) {
	// 	// We need to run instructions nbr_cycles times.
	// 	many_instruction_cycles(nbr_cycles, cpu);
	// } else {
	// 	// Execute the command passed in by the user.
	// 	done = exec_cmd(cmd_char, cmd_buffer, cpu);
	// }

	return done;
}


/*
 * Initialize the CPU (pc, ir, condition codes, running flag, 
 * GPRs).
 *
 * you should clear everything in the control unit to zero. 
 * The initial condition code should have zero flag set. Also clear all the
 * GPRs.
 */

void 
init_cpu (cpu_t *cpu) {
	// Reset CC, running and ir.
	cpu->cc = CC_ZER;
	cpu->running = 1;
	cpu->ir = 0;

	int i = 0;
	// Clear all GPRS
	for (i = 0; i < NREG; i++) {
		cpu->reg[i] = 0;
	}

}


/* 
 * Get the data file to initialize memory with.  If it was specified on the
 * command line as argv[1], use that file otherwise use default.hex.  If file
 * opening fails, complain and terminate program execution with an error.  See
 * linux command man 3 exit for details.
 */


FILE *
get_datafile (int argc, char *argv[]) {
	char *default_datafile_name = "default.hex";
	char *datafile_name;
	FILE *datafile = NULL;

	datafile_name = argv[1];

	printf("Loading %s\n", datafile_name);

	// open the file. If it couldn't be opened,
	// print "ERROR: could not open file.\n" and exit with
	// EXIT_FAILURE
	datafile = fopen(argv[1], "r");


	if (datafile) {
		return datafile;
	} else {
		printf("ERROR: could not open file.\n");
		exit(EXIT_FAILURE);
	}


}


/*
 * Read and dump initial values for memory
 *
 *
 *
 */
void 
init_memory (int argc, char** argv, cpu_t *cpu) {
	FILE *datafile = get_datafile(argc, argv);
	unsigned int value_read = 0;
	int i = 0;

	for (i = 0; i < MEMLEN; i++) {
		cpu->mem[i] = 0;
	}
	//  First set all of memory to zero.  Note: because addresses
	// are unsigned, we can't test for a address > MEMLEN; we
	// have to check for cycling back around to zero.

	// use read_hex_number() to read in the origin (this is
	// the first hex value in the file. If you couldn't read it, complain
	// with "ERROR: Couldn't read origin; quitting\n" and exit with EXIT FAILURE
	int hex_num = read_hex_number(datafile, &value_read);
	if (&value_read == NULL){
		printf("ERROR: Couldn't read origin; quitting\n");
		exit(3);
	}

	printf("Origin = x%04hX\n", (word_t) value_read);

	// set PC to the origin
	cpu->pc = value_read;
	int mem_i = cpu->pc;
	// use read_hex_number() repeatedly to
	// read in values from the file to fill in memory starting
	// at whatever address the origin was
	while(read_hex_number(datafile, &value_read)){
		cpu->mem[mem_i++] = value_read;
		if(mem_i >= MEMLEN){
			mem_i = 0;
		}
	}
	fclose(datafile);
	printf("\n");
}


/*
 * dump_cpu (cpu_t *cpu): Print out the control unit
 * and general-purpose registers (GPRs)
 *
 */

void 
dump_cpu (cpu_t *cpu) {
	printf("CPU STATE:\n");
	printf("PC = x%04hX   ", cpu->pc);
	printf("IR = x%04hX   ", cpu->ir);
	printf("CC = %c   ", cc2char(cpu));
	printf("RUNNING: %d\n", cpu->running);
	dump_gprs(cpu);
	printf("\n");
}


/* 
 * dump_memory(cpu_t *cpu, from, to): Print memory values
 * with addresses from, from+1, ..., to (possibly wrapping
 * around xFFFF to x0000).
 */

void
dump_memory (cpu_t *cpu, address_t from, address_t to) {
	if (to == (address_t) (from - 1)) {
		printf("MEMORY (from x%04hX):\n", from);
	} else {
		printf("MEMORY (locations x%04hX to x%04hX):\n", from, to);
	}

	address_t addr = 0;
	for (addr = from; addr != to ; addr++) {
		instr_t instr;
		decode_instr(cpu->mem[addr], &instr);
		print_addr_val_hex_dec_instr(addr, &instr);
	}

	instr_t instr;
	decode_instr(cpu->mem[to], &instr);
	print_addr_val_hex_dec_instr(addr, &instr);
	printf("\n");
}


/* 
 * dump_registers(cpu_t *cpu): Print register values in two rows of
 * five.
 */

void 
dump_gprs (cpu_t *cpu) {
	int regn;
	word_t w;
	for (regn = 0 ; regn < NREG ; regn++) {
		w = cpu->reg[regn];
		printf("R%d x%04hX %- 6d%s", regn, w, w, (regn % 4 == 3 ? "\n" : "   "));
	}
}


/*
 * Main program: Initialize the cpu, read in a program,
 * and execute it
 *
 * FILL ME IN: finish this function
 */

int
main (int argc, char **argv) {
	cpu_t cpu_value, *cpu = &cpu_value;

	printf("CS 350 Final Project: LC-3 Simulator ");

	if (argc != 2) {
		usage(argv);
	}

	init_cpu(cpu);

	init_memory(argc, argv, cpu);

	dump_cpu(cpu);
	dump_memory(cpu, cpu->pc, cpu->pc - 1); // dump from .ORIG

	printf("\nBeginning execution; type h for help\n");
	while(!read_exec_cmd(cpu));

	return 0;
}




