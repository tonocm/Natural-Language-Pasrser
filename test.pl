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

noun --> [apple].
noun --> [apples].
noun --> [boy].
noun --> [boys].
noun --> [girl].
noun --> [girls].
noun --> [government].
noun --> [governments].
noun --> [watermelon].
noun --> [watermelons].
noun --> [person].
noun --> [people].
det --> [a].
det --> [an].
det --> [the].
det --> [the].
det --> [any].
det --> [any].
det --> [all].
det --> [some].
det --> [some].
verb --> [conscript].
verb --> [conscripts].
verb --> [likes].
verb --> [like].
verb --> [run].
verb --> [runs].
beverb --> [is].
beverb --> [are].
adj --> [evil].
adj --> [big].
rel --> [that].
rel --> [whom].
rel --> [who].
rel --> [which].
rel --> [whose].
rel --> [when].
rel --> [when].
rel --> [where].
rel --> [where].
rel --> [why].
rel --> [why].
end --> [.].
end --> [?].
end --> [!].



s --> np, vp, sf.

np --> det, noun.
np --> det, noun, relcl.
np --> noun.

vp --> verb, np.
vp --> beverb, adj.
vp --> verb.

relcl --> rel, np, verb, np.
relcl --> rel, vp.
relcl --> rel, np, verb.

sf --> end.