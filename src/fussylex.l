%{/* -*- C++ -*- */
/* $Id: fussy.l,v 1.4 2006/08/05 20:07:41 sbhatnag Exp $ */
/******************************************************************
 * Copyright (c) 2000-2018, 2019 S.Bhatnagar
 *
 *  This file is part of fussy.
 *
 *  fussy is a free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  fussy is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with fussy.  If not, see <https://www.gnu.org/licenses/>.
 *

  History:

    First version.

               Sanjay Bhatnagar, May 2000
               sanjay@ncra.tifr.res.in

    Added tokens for "--" and "++" operators.    
               Sanjay Bhatnagar, Dec. 2003
               sbhatnag@aoc.nrao.edu

    Added the "setfmt" token.
               Sanjay Bhatnagar, March 10,2006
******************************************************************/

/* $Id: fussy.l,v 1.4 2006/08/05 20:07:41 sbhatnag Exp $ */
#include <stdio.h>
#include <string.h>
#include <calc.h>
#include <emath.h>
#include <units.h>
#include <string>
#include <list>
#include <signal.h>
#include <defns.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <IDResource.h>
#include <ErrorObj.h>

#undef  ECHO
#define LINELEN    256
#ifdef  LDBGMODE
#define ECHON     {fprintf(stderr,"\"\\n\"\n");}
#define ECHO      {fprintf(stderr,"\"%s\"\n",yytext);}
#define ECHOS(s)  {fprintf(stderr,"\"%s\"\n",s);}
#else
#define ECHON
#define ECHO
#define ECHOS(s) {}
#endif

char                        *Calc_line=NULL;
int                          Calc_index=0, GlobalFlag=0;
static string                qstr;
string                       Calc_prompt=">";
int                          FUSSY_IGNOREEOF=1e6;

extern IDResource            IDR;
extern string                Token;
extern VMac                  Prog;
extern list<Calc_Symbol>     ConstTab;
extern short int             InCStmt;
extern ErrorObj              MsgStream;// The global error message stream
extern Calc_Symbol*          GlobalDefaultFmt;
extern int                   InFuncDefn;
extern bool                  VMState_Quit;
#define INCR_INDEX           {Calc_index += yyleng;}
// Used by FLEX.  But using yy_scan_string instead of this now in FLEX 

#define YY_INPUT(buf,result,max_size) fussy_inp(buf,(int*)&result,max_size)
//
//---------------------------------------------------------------------
//
void stripwhite (char *string)
{
  register int i = 0;
  
  if (string!=NULL)
    {
      while (string[i] == ' ' || string[i]=='\t') i++;
      if (i) strcpy (string, string + i);
      i = strlen (string) - 1;
      while (i > 0 && (string[i]==' '||string[i]=='\t')) i--;
      string[++i] = '\0';
    }
}
//
//---------------------------------------------------------------------
//
void fussy_inp(char *buf, int *result, int max_size)
{                                   
  int N;
  static size_t Count=0;
  char *tt=NULL;

  if (GlobalFlag & FLAG_MORE_INP) Calc_prompt=" ";
  //  else Calc_prompt=">";
  else Calc_prompt=((tt=getenv("FUSSY_PROMPT"))==NULL?">":tt);

  if (Calc_line && (Count==strlen(Calc_line))) 
    {
      free(Calc_line);Count=0;
      Calc_line=NULL;
    }
  if (Calc_line==NULL) 
    {
      Calc_line=(char *) readline((char *)Calc_prompt.c_str());
      // If readline retuns NULL (e.g., when the input is Ctrl-D), print a message and
      // exit when FUSSY_IGNOREEOF<=0.
      if (Calc_line == NULL)
	{
	  if (FUSSY_IGNOREEOF > 0)
	    {
	      FUSSY_IGNOREEOF--;
	      fprintf(rl_outstream,"Use \'quit\' to exit.  Will ignore next %d EOF(s).\n",FUSSY_IGNOREEOF);
	    }
	  else
	    {
	      VMState_Quit=true;
	      // exit(0);
	      // ExitException e;throw(e);
	    }
	}
      if (Calc_line && strlen(Calc_line) && rl_instream==stdin)
        {
          stripwhite(Calc_line);
          add_history(Calc_line);
        }
    }
  if (Calc_line==NULL) {*result=YY_NULL;}
  else
    {
      N=strlen(Calc_line)-Count;
      N=max_size-1>N?N:max_size-1;
      strncpy(buf,&Calc_line[Count],N);
      Count += N;
      buf[N]='\n';
      //      *result=strlen(Calc_line);
      *result=N+1;
    }
  GlobalFlag |= FLAG_MORE_INP;
  // if (Calc_line != NULL)
  //   {
  //     free(Calc_line); Count=0;
  //     Calc_line=NULL;
  //   }
}

/* Used by LEX.
#define YY_INPUT() ((Calc_index==strlen(Calc_line))?0:Calc_line[Calc_index++])
*/
//#define unput(C) {Calc_line[Calc_index==0?Calc_index:--Calc_index]=C;}

//#define output(C) {fprintf(stderr,"%c",C);}
// yy_scan_buffer(Calc_line,strlen(Calc_line));

//
//-----------------------------------------------------------------
//
void intr_handler(int sig)
{
  if (sig==SIGINT) GlobalFlag |= FLAG_CTRL_C;
}
//
//-----------------------------------------------------------------
//
int calc(const char *s, edouble *v,FILE *instream, FILE *outstream)
{
  int i=0;
  yyleng=0;
  Calc_index=0;

  if (s) 
    {
      Calc_line = (char *)malloc(strlen(s)+1);
      strncpy(Calc_line,s, LINELEN);
      strcat(Calc_line,"\n");
      yy_scan_string(s);
    }
  else
    {
      if (instream)  
        {
          rl_instream=yyin=instream;
          rl_outstream=yyout=outstream;
          yyrestart(instream);
        }
      else
        {
          rl_instream=yyin=stdin;
          rl_outstream=yyout=stdout;
          yyrestart(yyin);
        }
    }
  i=yyparse();
  switch(i)
    {
    case SYNTAX_ERROR:
      {
	//        ywarn("syntax error", (char *)0);
        return -1;
        break;
      }
    case END_OF_INPUT: {return i;}
    default:
      {
        signal(SIGINT,intr_handler);
#if VMDBGMODE  
        prtVM();
#endif
        if ((unsigned int) ProgBase < Prog.size()) 
          *v=Run(Prog);
      }
    }

  return i;
}
//
//-----------------------------------------------------------------
//
void NumFromENum(char *eval, int &v_ndx, int &e_ndx)
{
  int i=0,n=strlen(eval);
  while (eval[i] == ' ')i++;

  v_ndx=i;
  while ((eval[i] != 'p') && (eval[i] != 'P') && (i < n)) i++;
  while ((eval[i] != 'm') && (eval[i] != 'M') && (i < n)) i++; i++;
  e_ndx = i;
}    
//
//-----------------------------------------------------------------
//
void split3(char *deg, int n, const char *tok, double *d)
{
  int j=0;
  string val;
  d[0]=d[1]=d[2]=0.0;
  while(j < n)
    {
      while ((deg[j] != tok[0]) &&(deg[j] != tok[1]) && 
             (deg[j] != tok[2]))
        val += deg[j++];
      if      (deg[j] == tok[0]) sscanf(val.c_str(),"%lf",&d[0]);
      else if (deg[j] == tok[1]) sscanf(val.c_str(),"%lf",&d[1]);
      else if (deg[j] == tok[2]) sscanf(val.c_str(),"%lf",&d[2]);
      val.resize(0);j++;
    }
}
//
//-----------------------------------------------------------------
//
void HandleExtendedNum(const char *Units, const double &ToRadians)
{/* (Extended ) Numbers in the hms format */
  double a[3]={0,0,0},b[3]={0,0,0};
  int i, j;

  ECHO;
  NumFromENum(yytext,i,j); 
  split3(&yytext[i],j-1,Units,a);
  //  if ((unsigned int)j < yyleng) 
  if (j < yyleng) 
    split3(&yytext[j],yyleng-j,Units,b);
  
  {
    Calc_Symbol d;
    BASIC_NUM err;
    //    d.type = NUMBER;
    d.type=0;SETBIT(d.type, NUMBER_TYPE);

    SETVAL(d.value,(a[0]+(a[1]+a[2]/60.0)/60.0)*ToRadians,
           (err=((b[0]+(b[1]+b[2]/60.0)/60.0)*ToRadians)));

    d.fmt=DEFAULT_FMT;
    //    d.otype.qstr=NULL;
    yylval.symb=installConst(d,(err!=0?-1:0));
  }
}
//
//-----------------------------------------------------------------
//
%}
DIGIT   [0-9]+
HEXD    0x[0-9a-fA-F]++
S       [\+\-]?
DOT     \.
PM      [pP][mM]
PMSIGN  "+/-"
POW     "**"
EQ      "=="
PS      ":="
NE      "!="
GT      ">"
LT      "<"
GE      ">="
LE      "<="
PE      "+="
OR      "|"
AND     "&"
NOT     "!"
PP      "++"
MM      "--"
DOTVAL  ".val"
DOTRMS  ".rms"
FORCE   "->"
XNUM    "xnum"
QUIT    (([Qq][Uu][Ii][Tt])|([Bb][Yy][Ee]))
FMT     (%({DIGIT}?"hms"|{DIGIT}?"dms"|"rad"|({S}?{NUM}?[fdeEcsxXoBbgG])))

ERROP   ({PM}|{PMSIGN})

SPACE   [ ]++
NUM     ({DIGIT}|{DIGIT}{DOT}|{DOT}{DIGIT}|{DIGIT}{DOT}{DIGIT})
CREAL   ({NUM}([Ee]{S}{DIGIT})?)
FREAL   ({NUM}([Dd]{S}{DIGIT})?)
RNUM    ({CREAL}|{FREAL})

HDEG    ({RNUM}h)
HMIN    ({RNUM}m)
HSEC    ({RNUM}s)
HANG    ({HDEG}?({SPACE}?{HMIN})?({SPACE}?{HSEC})?)

DDEG    ({RNUM}d)
DMIN    ({RNUM}\')
DSEC    ({RNUM}\")
DANG    ({DDEG}?({SPACE}?{DMIN})?({SPACE}?{DSEC})?)

ERNUM   ({RNUM}({SPACE}?{ERROP}{SPACE}?{RNUM})?)
EDANG   ({DANG}({SPACE}?{ERROP}{SPACE}?{DANG})?)
EHANG   ({HANG}({SPACE}?{ERROP}{SPACE}?{HANG})?)
EHEX    ({HEXD}({SPACE}?{ERROP}{SPACE}?{HEXD})?)
%x qstring comment
%option nounput
%option noyywrap
%%
[ \t]                    {INCR_INDEX;}
{QUIT}                   {ECHO;return FINISH;}
\/\*                     {
                           ECHO;
                           BEGIN(comment);
                         }
<comment>\*\/            {
                           ECHO;
                           BEGIN(INITIAL);
                         }
\"                       { /* Start of a quoted string*/
                           qstr.resize(0);
                           ECHO;
                           BEGIN(qstring);
                         }
<qstring>\"              { /* saw closing quote - all done */
                           BEGIN(INITIAL);
                           ECHO;
                           Calc_Symbol d;
                           d.type=0; SETBIT(d.type,QSTRING_TYPE);

                           d.fmt=DEFAULT_STRING_FMT;
                           //d.otype.qstr=&qstr;
                           d.qstr=qstr;
                             
                           yylval.symb=installConst(d,0);
                           //
                           // If this is a new string not found in the
                           // ConstTab, allocate a new mem. to hold
                           // it.  In this case, assign an ID of 0 to
                           // the corresponding symbol.  All QSTRINGS
                           // have an ID of 0 (since they don't
                           // participate in error propagation, they
                           // don't need a unique ID of their own).
                           //
                           // if (yylval.symb->otype.qstr==&qstr)
                           //   {
                           //     //       IDR.ReleaseID(yylval.symb->ID);
                           //     //       yylval.symb->ID=0;

                           //     yylval.symb->otype.qstr=new string;
                           //     *(yylval.symb->otype.qstr)=qstr;
                           //   }

                           return QSTRING;
                         }
<qstring>\n              { /* Error - unterminated string */
                           MsgStream << "###String error: string not terminated!" << endl;
                           BEGIN(INITIAL);
                           return 0;
                         }
<qstring>\\[0-7]{1,3}    { /* octal escape sequence */
                           int result;
                           (void) sscanf( yytext + 1, "%o", &result );
                           if ( result > 0xff )
                             {
                               MsgStream << "###String error: constant is out-of-bounds" 
                                    << endl;
                               BEGIN(INITIAL);
                             }
                           else (qstr) += result;
                         }
<qstring>\\n             {(qstr) += '\n';}
<qstring>\\t             {(qstr) += '\t';}
<qstring>\\r             {(qstr) += '\r';}
<qstring>\\b             {(qstr) += '\b';}
<qstring>\\f             {(qstr) += '\f';}
<qstring>\\(.|\n)        {(qstr) += yytext[1]; /* Escaped character */}
<qstring>[^\\\n\"]+      {(qstr)+= yytext; /* Every thing other than '\','\n','"'*/}


{FMT}                    { 
                           ECHO; 
                           {
                             Calc_Symbol d;
                             d.fmt=yytext;
                             d.type=0;SETBIT(d.type,FMT_TYPE);
                             //
                             // Since this type does not participate
                             // in error propagation, assign an ID of
                             // zero.
                             //
                             yylval.symb=installConst(d,0);
                             //            IDR.ReleaseID(yylval.symb->ID);
                             //            yylval.symb->ID=0;
                           }

                           yylval.symb->fmt=yytext;
                           INCR_INDEX;
                           return FMT;
                         }
{ERNUM}                  {/* Extended number format */ 
                           ECHO;
                           int v_ndx, e_ndx; 
                           double v=0,e=0;

                           NumFromENum(yytext,v_ndx,e_ndx);
                           sscanf(&yytext[v_ndx],"%lf",&v);
                           if (e_ndx < yyleng) 
                             sscanf(&yytext[e_ndx],"%lf",&e);

                           {
                             Calc_Symbol d;
                             //d.type = NUMBER;
                             d.type=0;SETBIT(d.type,NUMBER_TYPE);
                             SETVAL(d.value,v,e);

                             d.fmt=DEFAULT_FMT;
                             //d.otype.qstr=NULL;
                             //
                             // Create a new ID only if the associated error 
                             // is non-zero.
                             //
			     //			     cout << "In Func. def. = " << InFuncDefn << endl;
                             yylval.symb=installConst(d,(e!=0?-1:0));
                           }

                           INCR_INDEX;
                           return NUMBER;
                         }
{XNUM}                   {ECHO;return XNUM;}
{POW}                    {ECHO;                        return POW;} 
{EQ}                     {ECHO;                        return EQ; }
{FORCE}                  {ECHO;                        return FORCE;}
{PS}                     {ECHO;                        return PS; }
{NE}                     {ECHO;                        return NE; }
{GT}                     {ECHO;                        return GT; }
{LT}                     {ECHO;                        return LT; }
{LE}                     {ECHO;                        return LE; }
{GE}                     {ECHO;                        return GE; }
{PE}                     {ECHO;                        return PE; }
{OR}                     {ECHO;                        return OR; }
{AND}                    {ECHO;                        return AND;}
{NOT}                    {ECHO;                        return NOT;}
{PP}                     {ECHO;                        return PP;}
{MM}                     {ECHO;                        return MM;}
auto                     {ECHO;                        return AUTO;}
print                    {ECHO;yylval.Inst=print;      return PRINT;}
printn                   {ECHO;yylval.Inst=printn;     return PRINTN;}
if                       {ECHO;yylval.Inst=ifcode;     return IF;}
else                     {ECHO;                        return ELSE;}
while                    {ECHO;yylval.Inst=whilecode;  return WHILE;}
for                      {ECHO;yylval.Inst=forcode;    return FOR;}
break                    {ECHO;yylval.Inst=break_code; return BREAK;}
return                   {ECHO;yylval.Inst=ret;        return RETURN;}
help                     {ECHO;                        return HELP;}
func                     {ECHO;                        return FUNCDECL;}
proc                     {ECHO;                        return PROCDECL;}
warranty                 {ECHO;                        return WARRANTY;}
showsym                  {ECHO;                        return PRTSY;}
showcsym                 {ECHO;                        return CPRTSY;}
showlsym                 {ECHO;                        return CPRTLSY;}
showvm                   {ECHO;                        return PRTVM;}
showid                   {ECHO;                        return PRTID;}
prtss                    {ECHO;                        return PRTSS;}
css                      {ECHO;                        return CSS;}
time                     {ECHO;                        return TOD;}
mjd                      {ECHO;                        return MJD;}
fmjd                     {ECHO;                        return FMJD;}
lst                      {ECHO;                        return LST;}
day                      {ECHO;                        return GETDAY;}
month                    {ECHO;                        return GETMONTH;}
year                     {ECHO;                        return GETYEAR;}
setlong                  {ECHO;                        return SETLONG;}
setlat                   {ECHO;                        return SETLAT;}
setfmt                   {ECHO;                        return SETGFMT;}
[a-zA-Z_]+[a-zA-Z0-9_]*  {/* Variables/function names*/
                            Calc_Symbol *s;
                            ECHO;

                            INCR_INDEX;
                            Token=yytext;
                            if ((s=calcgetSymb(yytext))!=0) 
                              {
                                yylval.symb=s;
                                return gettype(s->type);
                              }
                            else {return UNDEF;}
                         }
{EHEX}                   {/* (Extended) Hex format numbers */
                            int e=0,v=0,i,j;
                            ECHO;
                            NumFromENum(yytext,i,j);
                            sscanf(&yytext[i],"%x",&v);
                            if (j<yyleng)
                              sscanf(&yytext[j-1],"%x",&e);

                            {
                              Calc_Symbol d;

                              d.type=0;SETBIT(d.type, NUMBER_TYPE);
                              SETVAL(d.value,v,e);

                              d.fmt=DEFAULT_FMT;
                              //d.otype.qstr=NULL;
                              yylval.symb=installConst(d,(e!=0?-1:0));
                            }
                            INCR_INDEX;
                            return NUMBER;
                         }
{EHANG}                  {/* (Extended ) Numbers in the hms format */
                           HandleExtendedNum("hms",F_H2R);
                           yylval.symb->units=U_RADIAN;
                           yylval.symb->fmt="%hms";
                           
                           INCR_INDEX;
                           return NUMBER;
                         }
{EDANG}                  {/* (Extended) Numbers in the d'" format */
                           HandleExtendedNum("d\'\"",F_D2R);
                           yylval.symb->units=U_RADIAN;
                           yylval.symb->fmt="%dms";
                           
                           INCR_INDEX;
                           return NUMBER;
                         }
<<EOF>>                  {return ENDOFINPUT;}
\n                       {
                          if (InCStmt) {ECHOS("CSP");  return CSP;      } 
                          else         {ECHOS(yytext); return yytext[0];}
                         }
{DOTVAL}                 {ECHO return GETVAL;}
{DOTRMS}                 {ECHO;return GETRMS;}
.                        {ECHO;return yytext[0];        }
