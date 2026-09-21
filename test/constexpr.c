#include "test.h"

float g40 = 1.5;
double g41 = 0.0 ? 55 : (0, 1 + 1 * 5.0 / 2 * (double)2 * (int)2.0);
float g42 = 2.25 + 3.75;
double g43 = -1.0 + (1 ? 9.0 / 2.0 : 0.0);
int g44 = 3.99;
int g45 = -3.99;
float g46 = 1 ? 7.25f : 0.0f;
double g47 = 0 ? 0.0 : (1, -2.0 * -4.0);
float g48 = (float)(3 + 4) / 2;
double g49 = (double)(unsigned)3 + 0.25;
int g50 = 7.75;
int g51 = 0 ? 1.25 : 8.75;
char g52[(int)(1.5 + 2.5)];

int main() {
  ASSERT(1, ({ int i=0; switch(3) { case 5-2+0*3: i++; } i; }));
  ASSERT(10, ({ enum { ten=1+2+3+4 }; ten; }));
  ASSERT(8, ({ int x[1+1]; sizeof(x); }));
  ASSERT(6, ({ char x[8-2]; sizeof(x); }));
  ASSERT(6, ({ char x[2*3]; sizeof(x); }));
  ASSERT(3, ({ char x[12/4]; sizeof(x); }));
  ASSERT(2, ({ char x[12%10]; sizeof(x); }));
  ASSERT(0b100, ({ char x[0b110&0b101]; sizeof(x); }));
  ASSERT(0b111, ({ char x[0b110|0b101]; sizeof(x); }));
  ASSERT(0b110, ({ char x[0b111^0b001]; sizeof(x); }));
  ASSERT(4, ({ char x[1<<2]; sizeof(x); }));
  ASSERT(2, ({ char x[4>>1]; sizeof(x); }));
  ASSERT(2, ({ char x[(1==1)+1]; sizeof(x); }));
  ASSERT(1, ({ char x[(1!=1)+1]; sizeof(x); }));
  ASSERT(1, ({ char x[(1<1)+1]; sizeof(x); }));
  ASSERT(2, ({ char x[(1<=1)+1]; sizeof(x); }));
  ASSERT(2, ({ char x[1?2:3]; sizeof(x); }));
  ASSERT(3, ({ char x[0?2:3]; sizeof(x); }));
  ASSERT(3, ({ char x[(1,3)]; sizeof(x); }));
  ASSERT(2, ({ char x[!0+1]; sizeof(x); }));
  ASSERT(1, ({ char x[!1+1]; sizeof(x); }));
  ASSERT(2, ({ char x[~-3]; sizeof(x); }));
  ASSERT(2, ({ char x[(5||6)+1]; sizeof(x); }));
  ASSERT(1, ({ char x[(0||0)+1]; sizeof(x); }));
  ASSERT(2, ({ char x[(1&&1)+1]; sizeof(x); }));
  ASSERT(1, ({ char x[(1&&0)+1]; sizeof(x); }));
  ASSERT(3, ({ char x[(int)3]; sizeof(x); }));
  ASSERT(15, ({ char x[(char)0xffffff0f]; sizeof(x); }));
  ASSERT(0x10f, ({ char x[(short)0xffff010f]; sizeof(x); }));
  ASSERT(4, ({ char x[(int)0xfffffffffff+5]; sizeof(x); }));
  ASSERT(8, ({ char x[(int*)0+2]; sizeof(x); }));
  ASSERT(12, ({ char x[(int*)16-1]; sizeof(x); }));
  ASSERT(3, ({ char x[(int*)16-(int*)4]; sizeof(x); }));

  ASSERT(4, ({ char x[(-1>>31)+5]; sizeof(x); }));
  ASSERT(255, ({ char x[(unsigned char)0xffffffff]; sizeof(x); }));
  ASSERT(0x800f, ({ char x[(unsigned short)0xffff800f]; sizeof(x); }));
  ASSERT(1, ({ char x[(unsigned int)0xfffffffffff>>31]; sizeof(x); }));
  ASSERT(1, ({ char x[(long)-1/((long)1<<62)+1]; sizeof(x); }));
  ASSERT(4, ({ char x[(unsigned long)-1/((long)1<<62)+1]; sizeof(x); }));
  ASSERT(1, ({ char x[(unsigned)1<-1]; sizeof(x); }));
  ASSERT(1, ({ char x[(unsigned)1<=-1]; sizeof(x); }));

  ASSERT(1, g40==1.5);
  ASSERT(1, g41==11);
  ASSERT(1, g42==6.0);
  printf("g43: %+3f\n", g43);
  printf("g45: %+3f\n", g45);
  ASSERT(1, g43==3.5);
  ASSERT(3, g44);
  ASSERT(-3, g45);
  ASSERT(1, g46==7.25);
  ASSERT(1, g47==8.0);
  ASSERT(1, g48==3.5);
  ASSERT(1, g49==3.25);
  ASSERT(7, g50);
  ASSERT(8, g51);
  ASSERT(4, sizeof(g52));
  ASSERT(4, ({ char x[1.5 + 2.5]; sizeof(x); }));
  ASSERT(4, ({ char x[(int)(1.5 + 2.5)]; sizeof(x); }));
  ASSERT(2, ({ char x[(int)2.9]; sizeof(x); }));
  ASSERT(5, ({ char x[2.0f+3.0f]; sizeof(x); }));
  ASSERT(7, ({ char x[1 ? 7.75 : 1.25]; sizeof(x); }));
  ASSERT(6, ({ char x[(0, 6.25)]; sizeof(x); }));
  ASSERT(4, ({ enum { four=1.5+2.5 }; four; }));
  ASSERT(5, ({ enum { five=(int)(2.5*2) }; five; }));
  ASSERT(8, ({ enum { eight=0 ? 1.25 : 8.75 }; eight; }));
  ASSERT(3, ({ int i=0; switch(3) { case 1.5+1.5: i=3; } i; }));

  printf("OK\n");
  return 0;
}
