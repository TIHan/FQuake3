/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/
// vm_x86.c -- load time compiler and execution environment for x86

#include "vm_local.h"

#ifdef __FreeBSD__ // rb0101023
#include <sys/types.h>
#endif

#ifndef _WIN32
#include <sys/mman.h> // for PROT_ stuff
#endif

/*

  eax	scratch
  ebx	scratch
  ecx	scratch (required for shifts)
  edx	scratch (required for divisions)
  esi	program stack
  edi	opstack

*/

// TTimo: initialised the statics, this fixes a crash when entering a compiled VM 
static	byte	*buf = NULL;
static	byte	*jused = NULL;
static	int		compiledOfs = 0;
static	byte	*code = NULL;
static	int		pc = 0;

static	int		*instructionPointers = NULL;

#define FTOL_PTR

#ifdef _WIN32

#if defined( FTOL_PTR )
int _ftol( float );
static	int		ftolPtr = (int)_ftol;
#endif

void AsmCall( void );
static	int		asmCallPtr = (int)AsmCall;

#else // _WIN32

#if defined( FTOL_PTR )
// bk001213 - BEWARE: does not work! UI menu etc. broken - stack!
// bk001119 - added: int gftol( float x ) { return (int)x; }

int qftol( void );     // bk001213 - label, see unix/ftol.nasm
int qftol027F( void ); // bk001215 - fixed FPU control variants
int qftol037F( void );
int qftol0E7F( void ); // bk010102 - fixed bogus bits (duh)
int qftol0F7F( void );


static	int		ftolPtr = (int)qftol0F7F;
#endif // FTOL_PTR

void doAsmCall( void );
static	int		asmCallPtr = (int)doAsmCall;
#endif // !_WIN32


static	int		callMask = 0; // bk001213 - init

static	int	instruction, pass;
static	int	lastConst = 0;
static	int	oc0, oc1, pop0, pop1;

typedef enum 
{
	LAST_COMMAND_NONE	= 0,
	LAST_COMMAND_MOV_EDI_EAX,
	LAST_COMMAND_SUB_DI_4,
	LAST_COMMAND_SUB_DI_8,
} ELastCommand;

static	ELastCommand	LastCommand;

/*
=================
AsmCall
=================
*/
#ifdef _WIN32
__declspec( naked ) void AsmCall( void ) {
int		programStack;
int		*opStack;
int		syscallNum;
vm_t*	savedVM;

__asm {
	mov		eax, dword ptr [edi]
	sub		edi, 4
	or		eax,eax
	jl		systemCall
	// calling another vm function
	shl		eax,2
	add		eax, dword ptr [instructionPointers]
	call	dword ptr [eax]
	mov		eax, dword ptr [edi]
	and		eax, [callMask]
	ret
systemCall:

	// convert negative num to system call number
	// and store right before the first arg
	neg		eax
	dec		eax

	push    ebp
	mov     ebp, esp
	sub     esp, __LOCAL_SIZE

	mov		dword ptr syscallNum, eax	// so C code can get at it
	mov		dword ptr programStack, esi	// so C code can get at it
	mov		dword ptr opStack, edi

	push	ecx
	push	esi							// we may call recursively, so the
	push	edi							// statics aren't guaranteed to be around
}

	savedVM = currentVM;

	// save the stack to allow recursive VM entry
	currentVM->programStack = programStack - 4;
	*(int *)((byte *)currentVM->dataBase + programStack + 4) = syscallNum;
//VM_LogSyscalls(  (int *)((byte *)currentVM->dataBase + programStack + 4) );
	*(opStack+1) = currentVM->systemCall( (int *)((byte *)currentVM->dataBase + programStack + 4) );

	currentVM = savedVM;

_asm {
	pop		edi
	pop		esi
	pop		ecx
	add		edi, 4		// we added the return value

	mov     esp, ebp
	pop     ebp

	ret
}

}

#else //!_WIN32

static	int		callProgramStack;
static	int		*callOpStack;
static	int		callSyscallNum;

void callAsmCall(void)
{
	vm_t	*savedVM;
	int		*callOpStack2;

	savedVM = currentVM;
	callOpStack2 = callOpStack;

	// save the stack to allow recursive VM entry
	currentVM->programStack = callProgramStack - 4;
	*(int *)((byte *)currentVM->dataBase + callProgramStack + 4) = callSyscallNum;
//VM_LogSyscalls(  (int *)((byte *)currentVM->dataBase + programStack + 4) );
	*(callOpStack2+1) = currentVM->systemCall( (int *)((byte *)currentVM->dataBase + callProgramStack + 4) );

 	currentVM = savedVM;
}

void AsmCall( void ) {
	__asm__("doAsmCall:      			\n\t" \
			"	movl (%%edi),%%eax			\n\t" \
			"	subl $4,%%edi				\n\t" \
			"   orl %%eax,%%eax				\n\t" \
			"	jl systemCall				\n\t" \
			"	shll $2,%%eax				\n\t" \
			"	addl %3,%%eax				\n\t" \
			"	call *(%%eax)				\n\t" \
		  " movl (%%edi),%%eax   \n\t" \
	    " andl callMask, %%eax \n\t" \
			"	jmp doret					   \n\t" \
			"systemCall:					\n\t" \
			"	negl %%eax					\n\t" \
			"	decl %%eax					\n\t" \
			"	movl %%eax,%0				\n\t" \
			"	movl %%esi,%1				\n\t" \
			"	movl %%edi,%2				\n\t" \
			"	pushl %%ecx					\n\t" \
			"	pushl %%esi					\n\t" \
			"	pushl %%edi					\n\t" \
			"	call callAsmCall			\n\t" \
			"	popl %%edi					\n\t" \
			"	popl %%esi					\n\t" \
			"	popl %%ecx					\n\t" \
			"	addl $4,%%edi				\n\t" \
			"doret:							\n\t" \
			"	ret							\n\t" \
			: "=rm" (callSyscallNum), "=rm" (callProgramStack), "=rm" (callOpStack) \
			: "rm" (instructionPointers) \
			: "ax", "di", "si", "cx" \
	);
}
#endif

static int	Constant4( void ) {
	int		v;

	v = code[pc] | (code[pc+1]<<8) | (code[pc+2]<<16) | (code[pc+3]<<24);
	pc += 4;
	return v;
}

static int	Constant1( void ) {
	int		v;

	v = code[pc];
	pc += 1;
	return v;
}

static void Emit1( int v ) 
{
	buf[ compiledOfs ] = v;
	compiledOfs++;

	LastCommand = LAST_COMMAND_NONE;
}

#if 0
static void Emit2( int v ) {
	Emit1( v & 255 );
	Emit1( ( v >> 8 ) & 255 );
}
#endif

static void Emit4( int v ) {
	Emit1( v & 255 );
	Emit1( ( v >> 8 ) & 255 );
	Emit1( ( v >> 16 ) & 255 );
	Emit1( ( v >> 24 ) & 255 );
}

static int Hex( int c ) {
	if ( c >= 'a' && c <= 'f' ) {
		return 10 + c - 'a';
	}
	if ( c >= 'A' && c <= 'F' ) {
		return 10 + c - 'A';
	}
	if ( c >= '0' && c <= '9' ) {
		return c - '0';
	}

	Com_Error( ERR_DROP, "Hex: bad char '%c'", c );

	return 0;
}
static void EmitString( const char *string ) {
	int		c1, c2;
	int		v;

	while ( 1 ) {
		c1 = string[0];
		c2 = string[1];

		v = ( Hex( c1 ) << 4 ) | Hex( c2 );
		Emit1( v );

		if ( !string[2] ) {
			break;
		}
		string += 3;
	}
}



static void EmitCommand(ELastCommand command)
{
	switch(command)
	{
		case LAST_COMMAND_MOV_EDI_EAX:
			EmitString( "89 07" );		// mov dword ptr [edi], eax
			break;

		case LAST_COMMAND_SUB_DI_4:
			EmitString( "83 EF 04" );	// sub edi, 4
			break;

		case LAST_COMMAND_SUB_DI_8:
			EmitString( "83 EF 08" );	// sub edi, 8
			break;
		default:
			break;
	}
	LastCommand = command;
}

static void EmitAddEDI4(vm_t *vm) {
	if (LastCommand == LAST_COMMAND_SUB_DI_4 && jused[instruction-1] == 0) 
	{		// sub di,4
		compiledOfs -= 3;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		return;
	}
	if (LastCommand == LAST_COMMAND_SUB_DI_8 && jused[instruction-1] == 0) 
	{		// sub di,8
		compiledOfs -= 3;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		EmitString( "83 EF 04" );	//	sub edi,4
		return;
	}
	EmitString( "83 C7 04" );	//	add edi,4
}

static void EmitMovEAXEDI(vm_t *vm) {
	if (LastCommand == LAST_COMMAND_MOV_EDI_EAX) 
	{	// mov [edi], eax
		compiledOfs -= 2;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		return;
	}
	if (pop1 == OP_DIVI || pop1 == OP_DIVU || pop1 == OP_MULI || pop1 == OP_MULU ||
		pop1 == OP_STORE4 || pop1 == OP_STORE2 || pop1 == OP_STORE1 ) 
	{	
		return;
	}
	if (pop1 == OP_CONST && buf[compiledOfs-6] == 0xC7 && buf[compiledOfs-5] == 0x07 ) 
	{	// mov edi, 0x123456
		compiledOfs -= 6;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		EmitString( "B8" );			// mov	eax, 0x12345678
		Emit4( lastConst );
		return;
	}
	EmitString( "8B 07" );		// mov eax, dword ptr [edi]
}

qboolean EmitMovEBXEDI(vm_t *vm, int andit) {
	if (LastCommand == LAST_COMMAND_MOV_EDI_EAX) 
	{	// mov [edi], eax
		compiledOfs -= 2;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		EmitString( "8B D8");		// mov bx, eax
		return qfalse;
	}
	if (pop1 == OP_DIVI || pop1 == OP_DIVU || pop1 == OP_MULI || pop1 == OP_MULU ||
		pop1 == OP_STORE4 || pop1 == OP_STORE2 || pop1 == OP_STORE1 ) 
	{	
		EmitString( "8B D8");		// mov bx, eax
		return qfalse;
	}
	if (pop1 == OP_CONST && buf[compiledOfs-6] == 0xC7 && buf[compiledOfs-5] == 0x07 ) 
	{		// mov edi, 0x123456
		compiledOfs -= 6;
		vm->instructionPointers[ instruction-1 ] = compiledOfs;
		EmitString( "BB" );			// mov	ebx, 0x12345678
		if (andit) {
			Emit4( lastConst & andit );
		} else {
			Emit4( lastConst );
		}
		return qtrue;
	}

	EmitString( "8B 1F" );		// mov ebx, dword ptr [edi]
	return qfalse;
}



