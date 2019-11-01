/* $Id: yyerror.c,v 1.3 2006/08/05 20:07:41 sbhatnag Exp $ */
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

******************************************************************/
#include "calc_lex_bison.h"
#include <stdio.h>
/*----------------------------------------------------------------------*/
int ywarn(char */*s*/, char */*t*/)
{
  return 1;
}
/*----------------------------------------------------------------------*/
int yymsg(const char *s, char *t)
{
  int i,N=0;
  const char *str = "###Error:";

  N += strlen(str)+strlen(s)+1;
  i=strlen(yytext);

  if (yytext[i-1]!='\n')
    fprintf(stderr, "%s %s near token \'%s\'", str,s,yytext);
  else
    fprintf(stderr, "%s %s", str,s);
  if (t) 
    {
      N += strlen(t)+1;
      fprintf(stderr, " %s", t);
    }
  N += 4;
  fprintf(stderr," in ");
  //  fprintf(stderr, "\"%s\"\n",GET_INP());
  fprintf(stderr, "\"%s\"",GET_INP());
  /*
  N = N + GET_INP_NDX() + yyleng+1;

  for (i=0;i<N; i++) fprintf(stderr," ");
  fprintf(stderr,"^\n");
*/
  fprintf(stderr,"\n");
  return 1;
}
/*----------------------------------------------------------------------*/
int yyerror(const char *s)
{
  yymsg(s, (char *) 0);
  return 0;
}
