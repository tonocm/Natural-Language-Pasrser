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
noun(noun(apples),p) --> [apples].
noun(noun(boy),s) --> [boy].
noun(noun(boys),p) --> [boys].
noun(noun(girl),s) --> [girl].
noun(noun(girls),p) --> [girls].
noun(noun(government),s) --> [government].
noun(noun(governments),p) --> [governments].
noun(noun(watermelon),s) --> [watermelon].
noun(noun(watermelons),p) --> [watermelons].
noun(noun(person),s) --> [person].
noun(noun(people),p) --> [people].
det(det(a),s) --> [a].
det(det(an),s) --> [an].
det(det(the),_) --> [the].
det(det(any),_) --> [any].
det(det(all),p) --> [all].
det(det(some),_) --> [some].
verb(verb(conscript),p) --> [conscript].
verb(verb(conscropts),s) --> [conscripts].
verb(verb(likes),s) --> [likes].
verb(verb(like),p) --> [like].
verb(verb(run),p) --> [run].
verb(verb(runs),s) --> [runs].
beverb(beVerb(is),s) --> [is].
beverb(beVerb(are),p) --> [are].
adj(adj(evil)) --> [evil].
adj(adj(big)) --> [big].
rel(relcl(that),s) --> [that].
rel(relcl(whom),s) --> [whom].
rel(relcl(who),p) --> [who].
rel(relcl(which),p) --> [which].
rel(relcl(whose),s) --> [whose].
rel(relcl(when),_) --> [when].
rel(relcl(where),_) --> [where].
rel(relcl(why),_) --> [why].
end --> [.].
end --> [?].
end --> [!].


sent(_) :-
  read_in(S0),
  s(T,S0,[]).
  
s(s(NP,VP)) --> np(NP,Num), vp(VP,Num), sf.

np(np(DET,N),Num) --> det(DET,Num), noun(N,Num).
np(np(DET,N,R),Num) --> det(DET,Num), noun(N,Num), relcl(R,Num).
np(np(N),Num) --> noun(N,Num).

vp(vp(V,NP),Num) --> verb(V,Num), np(NP,_).
vp(vp(BEV,A),Num) --> beverb(BEV,Num), adj(A).
vp(vp(V),Num) --> verb(V,Num).

%test for first relcl cus it might not work because of NP2
relcl(relcl(R,NP,V,NP2),Num) --> rel(R,Num), np(NP,Num), verb(V,Num), np(NP2,_).
relcl(relcl(R,VP),Num) --> rel(R,Num), vp(VP,Num).
relcl(relcl(R,NP,V),Num) --> rel(R,Num), np(NP,Num), verb(V,Num).

sf --> end.