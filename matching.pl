vertical(line(point(X,Y),point(X,Z))).
horizontal(line(point(X,Y),point(Z,Y))).

word(abalone,a,b,a,l,o,n,e).
word(abandon,a,b,a,n,d,o,n).
word(enhance,e,n,h,a,n,c,e).
word(anagram,a,n,a,g,r,a,m).
word(connect,c,o,n,n,e,c,t).
word(elegant,e,l,e,g,a,n,t).

crossword(V1,V2,V3,H1,H2,H3) :-
  word(V1,L11,L12,L13,L14,L15,L16,L17),
  word(V2,L21,L22,L23,L24,L25,L26,L27),
  word(V3,L31,L32,L33,L34,L35,L36,L37),
  word(H1,L41,L12,L43,L22,L45,L32,L47),
  word(H2,L51,L14,L53,L24,L55,L34,L57),
  word(H3,L61,L16,L63,L26,L56,L36,L67).
