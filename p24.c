#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <termios.h>

uint8_t  limit = 15;
uint16_t clock = 0;
uint16_t range = 0x1fff;

uint32_t t = 0,    /* 25 bit */
         r = 0,    /* 25 bit */
         a = 0,    /* 25 bit */
    ds[15] = {0},  /* 25 bits wide */
    rs[15] = {0},  /* 25 bits wide */
         p = 0,    /* 24 bit */
         i = 0,    /* 24 bit */
	 * mem;   /* 24 bits wide */

uint8_t sp = 0,    /* 8 bits */
        rp = 0,    /* 8 bits */
   slot[4] = {0};  /* instruction cache 8 bits */

uint32_t _t = 0,   /* 25 bit */
	 _r = 0,   /* 25 bit */
         _a = 0,   /* 25 bit */
	 _p = 0,   /* 24 bit */
	 _i = 0;   /* 24 bit */

uint8_t _sp = 0,   /* 8 bits */
        _rp = 0,   /* 8 bits */
   _slot[4] = {0}; /* 8 bits */

static char getch(void) { /* reads from keypress, doesn't echo */
    struct termios oldattr, newattr;
    char ch;
    tcgetattr( STDIN_FILENO, &oldattr );
    newattr = oldattr;
    newattr.c_iflag &= ~( ICRNL );
    newattr.c_lflag &= ~( ICANON | ECHO );
    tcsetattr( STDIN_FILENO, TCSANOW, &newattr );
    ch = getchar();
    tcsetattr( STDIN_FILENO, TCSANOW, &oldattr );
    // printf("%d\n", ch);
	if(ch==0x1b) exit(0);
    return ch==127 ? 8 : ch;
}
static int putch(char c) { /* output character to sstdout & flush */
    int res=putchar(c);
    fflush(stdout);
    return res;
}
static void Cycle(void) {
  i = _i; t =_t; r = _r; p = _p; a = _a; rp = _rp; sp = _sp;  
  slot[3] = _slot[3];
  slot[2] = _slot[2];
  slot[1] = _slot[1];
  slot[0] = _slot[0];
  clock++;
 }
static void next(void) { clock |= 7; }
static void rpush(uint32_t v) { _rp = (limit&(rp+1)); rs[_rp] = r; _r = v; }
static uint32_t rpop(void) { _rp = (limit&(rp-1)); _r = rs[rp]; return r; }
static void spush(uint32_t v) { _sp = (limit&(sp+1)); ds[_sp] = t; _t = v; }
static uint32_t spopp(void) { _sp = (limit&(sp-1)); _t = ds[sp];  return t; }
static void _continue() {
  _i = mem[p];
  _slot[3] = (_i%64);
  _slot[2] = ((_i/64)%64);
  _slot[1] = (((_i/64)/64)%64);
  _slot[0] = (63&(((_i/64)/64)/64));
  _p = (range&(p+1));
}
static void jmp   (){ _p=(range&i); next(); };
static void call  (){ rpush(p); jmp(); };
static void ret   (){ _p=(range&rpop()); next(); };
static void jz    (){ (spopp()&0xffffff) ? next() : jmp(); };
static void jnc   (){ (t&0x1000000) ? next() : jmp(); };
static void ld    (){ spush(mem[(range&a)]); };
static void ldp   (){ ld(); _a=a+1; };
static void ldi   (){ _p=(range&(p+1)); spush(mem[(range&p)]); };
static void st    (){ mem[(range&a)]=spopp(); };
static void stp   (){ st(); _a=a+1; };
static void com   (){ _t=((t&0xffffff)^0xffffff); };
static void shr   (){ _t=((t>>1)&0xffffff); };
static void shl   (){ _t=((t<<1)&0x1ffffff); };
static void mult  (){ 
    int y = (a&1) ? ((ds[sp]+t)&0x1ffffff) : t;
	 _t = (y>>1);
    _a = (y&1) ? (((a&0xffffff)>>1)|0x800000) : ((a&0xffffff)>>1);
};
static void andd  (){ _t=((spopp()&_t)&0xffffff); };
static void xorr  (){ _t=((spopp()^_t)&0xffffff); };
static void ddiv  (){  
    int y = 0, x = (((ds[sp]&0xffffff)+(t&0xffffff)));
    if ((x&0x1000000)) {
       y = ((x&0xffffff)<<1);
       _a = (((a<<1)&0xffffff)+1);
    } else { 
       y = ((t&0xffffff)<<1); 
      _a = (((a<<1)&0xffffff));
    }
    _t = (a&0x800000) ? y+1 : y;
};
static void add   (){ _t=((spopp()&0xffffff)+(_t&0xffffff)); };
static void popr  (){ spush(rpop()); };
static void pushs (){ spush(t); };
static void lda   (){ spush(a); };
static void pushr (){ rpush(spopp()); };
static void sta   (){ _a=spopp(); };
static void pops  (){ spopp(); };
static void nop   (){ next(); };
static void get   (){ char c = getch(); spush(c); ret(); };
static void put   (){ char c = spopp(); putch((c&0x7f)); ret(); };
static void execute(uint8_t code)
{
  switch(code) {
  case 0x00: jmp();   break;
  case 0x01: ret();   break;
  case 0x02: jz();    break;
  case 0x03: jnc();   break;
  case 0x04: call();  break;
  case 0x06: get();   break;
  case 0x07: put();   break;
  case 0x09: ldp();   break;
  case 0x0a: ldi();   break;
  case 0x0b: ld();    break;
  case 0x0d: stp();   break;
  case 0x0f: st();    break;
  case 0x10: com();   break;
  case 0x11: shl();   break;
  case 0x12: shr();   break;
  case 0x13: mult();  break;
  case 0x14: xorr();  break;
  case 0x15: andd();  break;
  case 0x16: ddiv();  break;
  case 0x17: add();   break;
  case 0x18: popr();  break;
  case 0x19: lda();   break;
  case 0x1a: pushs(); break;
  case 0x1c: pushr(); break;
  case 0x1d: sta();   break;
  case 0x1e: nop();   break;
  case 0x1f: pops();  break;
  }
}
static void sstack()
{
   printf(" s:%0x ", t);
   int x = 0;
   for(x = sp; x > 0 ; x--)
     printf("%0x ", ds[x]);
   printf("\n");
}
static void rstack()
{
   printf(" r:%0x ", r);
   int x;
   for(x = rp; x > 0; x--)
     printf("%0x ", rs[x]);
   printf("\n");
}
static void registers()
{
  printf(" p=%0x i=%0x i1=%0x i2=%0x i3=%0x i4=%0x sp=%0x rp=%0x \n a=%0x\n",
      p, i, slot[0], slot[1], slot[2], slot[3], sp, rp, a);
}
static void S() { printf("\n clock=%0x", clock); registers(); sstack(); rstack(); }
static void Sync()
{
  switch(clock&7) {
  case 0: _continue(); break;
  case 1: execute(slot[0]); break;
  case 2: execute(slot[1]); break;
  case 3: execute(slot[2]); break;
  case 4: execute(slot[3]);
  default: next(); break;
  }
}
static void c() { Sync(); Cycle(); S(); }
int main()
{
  uint32_t memory[0x8000];
  FILE *f = fopen("p24.bin", "rb");
  fread(memory, 8192, sizeof(memory[0]), f);
  mem = memory;
  while(1) { Sync(); Cycle(); }
}
