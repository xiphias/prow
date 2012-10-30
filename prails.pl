z(3).
z(4).

row(Row) :-
   b(D),
   z(X),
   content(D, X),
   Row=[
	   'Hello, ',
	   D,
	   '<br>'].

% row ::
%  hbox @ (
%   string('Hello, '),
%   b @ (
%     content: z),
%   string('<br'>)).

rows(Rows) :-
  bagof(R, row(R), Rows).

% rows ::
%   bagof(R, row(R)).

  
show2(X, S) :-
  rows(Rows),
      render(
     Rows, S),
	show(S).


tag(Name, tag(T)) :-
  cmember(name(Name), T).

b(B) :- tag(b, B).
content(tag(A),Text) :-
   cmember(content(Text), A).




to_simple_array([], []).
to_simple_array([A | B], Z) :-
  ground(A), !, Z=[A | C], to_simple_array(B, C).
to_simple_array([A|_], []).

cmember(A, B) :- member(A, B), !.
	
show(S) :- 
  telling(OldOut),
  tell('out.html'),
  write(S),
  told,
  tell(OldOut).
  

render([H | F], S) :-
  nonvar(H),
  !,
  render(F, S2),
  render(H, HR),
  concat(HR, S2, S).
  
render([], S) :- !, S = ''.


render([A|_], S) :- S = ''.
  

 vmember(A, [A]) :- ground(A), !.
 vmember(A, [A|_]) :- ground(A), !.
 vmember(A, [B|C]) :-
   ground(B), vmember(A, C).
 

 
render(tag(D), S) :- !,
   vmember(name(Name), D),
   vmember(content(Text), D),
   render(['<', Name, '>', Text,
     '</', Name, '>'], S).
 
 
render(H, HR) :-
   atomic(H), !, H = HR.
 
 appendarg(S, Arg, L) :-
   S =.. SL,
   append(SL, [Arg], LL),
   L =.. LL.

:- op(600, xfy, ::).
:- op(600, xfy, .).
   
 ::(A,B) :-
   transform(A::B, T),
   assert(T).
 
 list_to_comma([A], B) :- !, A=B.
 list_to_comma([A|B], (A,L)) :-
   list_to_comma(B, L).
 
 extractattributes([], _, []).
 extractattributes([A:B|L], Out, E) :- !,
   Clause =.. [B, Z],
   extractattributes(L, Out, LE),
   E = [attribute(Out, A, Z),
     Clause | LE].

 extractattributes([(A.B)|L], Out, E) :- !,
   B =.. [Name | Attributes],
   Clause =.. [Name, A, AN],
   extractattributes(Attributes, AN, EAttributes),
   AE = [container(Out, AN), Clause | EAttributes],
   extractattributes(L, Out, LE),
   append(AE, LE, E).
	 
 extractattributes([A|L], Out, E) :-
   A =.. [Name | Attributes],
   Clause =.. [Name, AN],
   extractattributes(Attributes, AN, EAttributes),
   AE = [container(Out, AN), Clause | EAttributes],
   extractattributes(L, Out, LE),
   append(AE, LE, E).
 
 
 transformelem((E.F), Out, T) :- !,
   F =.. [H | P],
   EO =.. [H, E, Out],
   extractattributes(P, Out, Ex),
   list_to_comma([EO|Ex], T).
   
 
 
 transformelem(E, Out, T) :-
   E =.. [H | P],
   EO =.. [H, Out],
   extractattributes(P, Out, Ex),
   list_to_comma([EO|Ex], T).
   
 
 transform(B::C, BO:-CO) :- 
   appendarg(B, Out, BO),
   transformelem(C, Out, CO).
 
 check(A) :-
   call(A) ,!.
   
 check(A) :-  throw(hello).
   
 :- check(transform(main::b, main(A):-b(A))).
 :- check(transform(
   main::header(background_color:blue),
   main(Out) :- (
     header(Out),
     attribute(Out, background_color, C),
	 blue(C)))).
	 
	 
:- check(transform( 
main::
  vbox(
    header(background_color:blue),
	hbox(
	  vbox(
	    me.small,
		all_pages)),
      current_page),

  main(V) :- (
    vbox(V),
	container(V, Header),
	header(Header),
	attribute(Header, background_color, B),
	blue(B),
	container(V, HBox),
	hbox(HBox),
	container(HBox, V2),
	vbox(V2),
	container(V2, MS),
  	small(me, MS),
	container(V2, AllPages),
	all_pages(AllPages),
	container(HBox, CurrentPage), 
	current_page(CurrentPage)
	  ))).
 
 
 big_planes(A,B) :- zzzzz(B,A).
 