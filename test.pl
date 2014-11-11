clear :- write('\e[H\e[2J').
exit :- halt.
/* Code from the section 5.3 of the textbook,
      Programming in Prolog by Clocksin & Mellish.

      The function read_in(S) takes a raw sentence via
      the standard input, and the sentence's list form
      is assigned to the variable S. For example, with
      the input
         A watermelon contains a divine flavor!
      we will obtain
         S = [a,watermelon,contains,a,divine,flavor,!]
      so that we don't have to bother ourselves with
      converting a sentence into a list whenever
      we try to parse it. Note that all capial letters
      become lowercase. Also, it assumes the input
      sentence ends with either '.', '?', or '!'. */

read_in([W|Ws]) :- get_char(C),
                     readword(C,W,C1),
                                        restsent(W,C1,Ws).

restsent(W, _, []) :- lastword(W),!.
restsent(_, C, [W1|Ws]) :- readword(C,W1,C1),
                     restsent(W1,C1,Ws).

readword(C,C,C1) :- single_character(C),!,get_char(C1).
readword(C,W,C2) :-
  in_word(C, NewC),
  !,
  get_char(C1),
  restword(C1,Cs,C2),
  atom_chars(W,[NewC|Cs]).
readword(_,W,C2):-get_char(C1),readword(C1,W,C2). % skipping blanks.

restword(C,[NewC|Cs],C2) :-
  in_word(C,NewC),
  !,
  get_char(C1),restword(C1,Cs,C2).
restword(C,[],C).

in_word(C,C) :- letter(C,_).
in_word(C,L) :- letter(L,C).
in_word(C,C) :- digit(C).
in_word(C,C) :- special_character(C).

special_character('-').
special_character('''').

single_character(',').          single_character(':').
single_character('.').          single_character('?').
single_character(';').          single_character('!').

letter(a,'A').  letter(n,'N').
letter(b,'B').  letter(o,'O').
letter(c,'C').  letter(p,'P').
letter(d,'D').  letter(q,'Q').
letter(e,'E').  letter(r,'R').
letter(f,'F').  letter(s,'S').
letter(g,'G').  letter(t,'T').
letter(h,'H').  letter(u,'U').
letter(i,'I').  letter(v,'V').
letter(j,'J').  letter(w,'W').
letter(k,'K').  letter(x,'X').
letter(l,'L').  letter(y,'Y').
letter(m,'M').  letter(z,'Z').

digit('0').        digit('5').
digit('1').        digit('6').
digit('2').        digit('7').
digit('3').        digit('8').
digit('4').        digit('9').

lastword('.').
lastword('!').
lastword('?').

noun(noun(apple),s) --> [apple].
noun(noun(apple),p) --> [apples].
noun(noun(boy),s) --> [boy].
noun(noun(boy),p) --> [boys].
noun(noun(girl),s) --> [girl].
noun(noun(girl),p) --> [girls].
noun(noun(flavor),s) --> [flavor].
noun(noun(flavor),p) --> [flavors].
noun(noun(government),s) --> [government].
noun(noun(government),p) --> [governments].
noun(noun(watermelon),s) --> [watermelon].
noun(noun(watermelon),p) --> [watermelons].
noun(noun(person),s) --> [person].
noun(noun(person),p) --> [people].
det(determiner(a),s) --> [a].
det(determiner(an),s) --> [an].
det(determiner(all),p) --> [all].
det(determiner(the),_) --> [the].
det(determiner(any),_) --> [any].
det(determiner(all),p) --> [all].
det(determiner(some),_) --> [some].
verb(verb(conscript),p) --> [conscript].
verb(verb(conscript),s) --> [conscripts].
verb(verb(like),s) --> [likes].
verb(verb(like),p) --> [like].
verb(verb(contain),p) --> [contain].
verb(verb(contain),s) --> [contains].
verb(verb(run),p) --> [run].
verb(verb(run),s) --> [runs].
verb(verb(eat),s) --> [eats].
verb(verb(eat),p) --> [eat].
beverb(be(is),s) --> [is].
beverb(be(is),p) --> [are].
adj(adjective(evil)) --> [evil].
adj(adjective(pacifist)) --> [pacifist].
adj(adjective(big)) --> [big].
adj(adjective(divine)) --> [divine].
adj(adjective(delicious)) --> [delicious].
rel(relatve(that),_) --> [that].
rel(relative(whom),s) --> [whom].
rel(relative(who),p) --> [who].
rel(relative(which),p) --> [which].
rel(relative(whose),s) --> [whose].
rel(relative(when),_) --> [when].
rel(relative(where),_) --> [where].
rel(relative(why),_) --> [why].
end --> [.].
end --> [?].
end --> [!].

glue([all],'=>','all(x').
glue([some],'&','exists(x').


sent(X) :-
  read_in(S0),
  %S0 = [all,boys,like,some,girls,'.'],
  s(X,S0,[]),
  %write(X),
  tr_s(X,Y,1),
  write(Y).

tr_s(X,Y,Var) :-
  X =.. List,
  append([sentence],[NP,VP],List),
  tr_np(NP,Y1,Var,Glue),
  tr_vp(VP,Y2,Var),
  atom_concat(Y1,' ',Temp),
  atom_concat(Temp,Glue,Temp1),
  atom_concat(Temp1,' ',Temp2),
  atom_concat(Temp2,Y2,Temp3),
  atom_concat(Temp3,')',Y).

tr_vp(VP,Y,Var) :-
  VP =.. List,
  append([verb_phrase],[V],List),
  tr_verb(V,Y1,Var),
  atom_concat(Y1,')',Y).

tr_vp(VP,Y,Var) :-
  VP =.. List,
  append([verb_phrase],[_,ADJ],List),
  tr_adj(ADJ,Y,Var).

tr_adj(ADJ,Y,Var) :-
  ADJ =.. List,
  append([adjective],[Ad],List),
  atom_concat(Ad,'(x',Temp),
  atom_concat(Temp,Var,Temp2),
  atom_concat(Temp2,')',Y).

tr_vp(VP,Y,Var) :-
  VP =.. List,
  append([verb_phrase],[V,NP],List),
  tr_verb(V,Y1,Var),
  Var2 is Var+1,
  tr_np(NP,Y2,Var2,_),%anonymous variable instead of Glue
  atom_concat(Y1,', ',Temp),
  atom_concat(Temp,Y2,Temp2),
  atom_concat(Temp2,'))',Y).                             

tr_verb(V,Y,Var) :-
  V =.. List,
  append([verb],[Verb],List),
  atom_concat(Verb,'(x',Temp),
  atom_concat(Temp,Var,Y).

tr_np(NP,Y,Var,Glue) :-
  NP =.. List,
  append([noun_phrase],[DET,Noun],List),
  tr_det(DET,Y1,Var,Glue),
  tr_noun(Noun,Y2,Var),
  atom_concat(Y1,Y2,Y).

tr_np(NP,Y,Var,Glue) :-
  NP =.. List,
  append([noun_phrase],[Det,Adj,Noun],List),
  tr_det(Det,Y1,Var,Glue),
  tr_adj(Adj,Y2,Var),
  tr_noun(Noun,Y3,Var),
  atom_concat(Y1,Y2,Temp),
  atom_concat(Temp,'& ',Temp2),
  atom_concat(Temp2,Y3,Y).

tr_np(NP,Y,Var,Glue) :-
  NP =.. List,
  append([noun_phrase],[Det,Noun,Relcl],List),
  tr_det(Det,Y1,Var,Glue),
  tr_noun(Noun,Y2,Var),
  tr_relcl(Relcl,Y3,Var),
  atom_concat(Y1,Y2,Temp),
  atom_concat(Temp,Y3,Y).

tr_det(DET,Y,Var,Glue) :-
  DET =.. List,
  append([determiner],Out,List),
  glue(Out,Glue,Y2),
  atom_concat(Y2,Var,Y3),
  atom_concat(Y3,', ',Y).

tr_adj(Adj,Y,Var) :-
  Adj =.. List,
  append([adjective],[Adjective],List),
  atom_concat(Adjective,'(x',Temp),
  atom_concat(Temp,Var,Temp2),
  atom_concat(Temp2,')',Y).
  
tr_noun(N,Y,Var) :-
  N =.. List,
  append([noun],[Noun],List),
  atom_concat(Noun,'(x',Temp),
  atom_concat(Temp,Var,Temp2),
  atom_concat(Temp2,')',Y).

tr_rel(' & ').
  
tr_relcl(Relcl,Y,Var) :-
  Relcl =.. List,
  append([rel_clause],[_,VP],List),
  tr_rel(Y1),
  tr_vp(VP,Y2,Var),
  atom_concat(Y1,Y2,Y).

tr_relcl(Relcl,Y,Var) :-
  Relcl =.. List,
  append([rel_clause],[_,NP,V],List),
  tr_rel(Y1),
  Var2 is Var + 1,
  tr_np(NP,Y2,Var2,Glue),
  tr_verb(V,Y3,Var2),
  atom_concat(Y1,Y2,Temp),
  atom_concat(Temp,Glue,Temp2),
  atom_concat(Temp2,Y3,Temp3),
  atom_concat(Temp3,')',Y).


s(sentence(NP,VP)) --> np(NP,Num), vp(VP,Num), sf.

np(noun_phrase(DET,N),Num) --> det(DET,Num), noun(N,Num).
np(noun_phrase(DET,A,N),Num) --> det(DET,Num), adj(A), noun(N,Num).
np(noun_phrase(DET,N,R),Num) --> det(DET,Num), noun(N,Num), relcl(R,Num).
np(noun_phrase(A,N),Num) --> adj(A), noun(N,Num).
np(noun_phrase(N),Num) --> noun(N,Num).

vp(verb_phrase(V,NP),Num) --> verb(V,Num), np(NP,_).
vp(verb_phrase(BEV,A),Num) --> beverb(BEV,Num), adj(A).
vp(verb_phrase(V),Num) --> verb(V,Num).

%test for first relcl cus it might not work because of NP2
relcl(rel_clause(R,NP,V,NP2),Num) --> rel(R,Num), np(NP,Num), verb(V,Num), np(NP2,_).
relcl(rel_clause(R,VP),Num) --> rel(R,Num), vp(VP,Num).
relcl(rel_clause(R,NP,V),Num) --> rel(R,Num), np(NP,Num), verb(V,Num).

sf --> end.