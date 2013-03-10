(***************************** PRIMA PARTE *****************************)

(*
Librerie non (piu') fondamentali
load "Int";
open Int;
*)

datatype token = LET | IN | END | LETREC | AND |
                LAMBDA | OP | ID | SYM | NM | STR | BOOL | Nil| Notoken

datatype s_espressione = NUM of int |
                        STRINGA of string |
                        T | F | NIL |
						DOT of s_espressione * s_espressione

datatype meta_S = M of s_espressione | S of string

datatype Sexpr = Var of string |
                 Quote of s_espressione |
                 Op of string * Sexpr list |
                 If of Sexpr * Sexpr * Sexpr |
                 Lambda of string list * Sexpr |
				(*Lambda of string list * Sexpr |*)
                 Call of Sexpr * Sexpr list |
                 Let of Sexpr * string list * Sexpr list |
                 Letrec of Sexpr * string list * Sexpr list


type token_lexema = token * meta_S

exception e of string;

(* tipo RES per gestire i dati ritornati dalle varie funzioni
string1 = lexema ottenuto con i caratteri parsati
string2 = sottostringa rimanente
token = token relativo al lexema string1 *)

datatype result = RES of string * string * token

fun res1(RES(x,y,z))=x
fun res2(RES(x,y,z))=y
fun res3(RES(x,y,z))=z


(************** isletter **************)
fun isletter(c: char): bool =
	if c = #"a" orelse c = #"b" orelse c = #"c" orelse c = #"d" orelse c = #"e"
				orelse c = #"f" orelse c = #"g" orelse c = #"h" orelse c = #"i"
				orelse c = #"j" orelse c = #"k" orelse c = #"l" orelse c = #"m"
				orelse c = #"n" orelse c = #"o" orelse c = #"p" orelse c = #"q"
				orelse c = #"r" orelse c = #"s" orelse c = #"t" orelse c = #"u"
				orelse c = #"v" orelse c = #"w" orelse c = #"x" orelse c = #"y"
				orelse c = #"z" orelse c = #"A" orelse c = #"B" orelse c = #"C" 
				orelse c = #"D" orelse c = #"E" orelse c = #"F" orelse c = #"G"
				orelse c = #"H" orelse c = #"I"	orelse c = #"J" orelse c = #"K"
				orelse c = #"L" orelse c = #"M"	orelse c = #"N" orelse c = #"O"
				orelse c = #"P" orelse c = #"Q"	orelse c = #"R" orelse c = #"S"
				orelse c = #"T" orelse c = #"U" orelse c = #"V" orelse c = #"W"
				orelse c = #"X" orelse c = #"Y"	orelse c = #"Z" orelse c = #"_"
	then true
	else false;


(************** isdigit **************)
fun isdigit(c: char): bool =
	case c of
	#"0" => true |
	#"1" => true |
	#"2" => true |
	#"3" => true |
	#"4" => true |
	#"5" => true |
	#"6" => true |
	#"7" => true |
	#"8" => true |
	#"9" => true |
	_ => false;


(************** issymbol **************)
fun issymbol(c: char): bool =
	case c of
	#"(" => true |
	#")" => true |
	#"[" => true |
	#"]" => true |
	#"=" => true |
	#":" => true |
	#"$" => true |
	_ => false;


(************** check_digit **************)
fun check_digit(substr: string, []: char list): result = RES("", "", NM)
|	check_digit(substr: string, h::tail: char list): result =
    if isdigit(h) then check_digit(substr^str(h), tail)
    else RES(substr, implode(h::tail), NM);


(************** check_letter **************)
fun check_letter(substr: string, []: char list): result = RES(substr, "", Notoken)
| check_letter(substr: string, h::tail: char list): result =
    if isletter(h) then check_letter(substr^str(h), tail)
	else if isdigit(h) then check_letter(substr^str(h), tail)
	else if (substr="let") then RES(substr, implode(h::tail), LET)
	else if (substr="in") then RES(substr, implode(h::tail), IN)
	else if (substr="end") then RES(substr, implode(h::tail), END)
	else if (substr="letrec") then RES(substr, implode(h::tail), LETREC)
	else if (substr="and") then RES(substr, implode(h::tail), AND)
	else if (substr="NIL") then RES(substr, implode(h::tail), Nil)
	else if (substr="T") then RES(substr, implode(h::tail), BOOL)
	else if (substr="F") then RES(substr, implode(h::tail), BOOL)
	else if (substr="LAMBDA") then RES(substr, implode(h::tail), LAMBDA)
	else if (substr="ADD") then RES(substr, implode(h::tail), OP)
	else if (substr="SUB") then RES(substr, implode(h::tail), OP)
	else if (substr="MUL") then RES(substr, implode(h::tail), OP)
	else if (substr="DIV") then RES(substr, implode(h::tail), OP)
	else if (substr="REM") then RES(substr, implode(h::tail), OP)
	else if (substr="EQ") then RES(substr, implode(h::tail), OP)
	else if (substr="LEQ") then RES(substr, implode(h::tail), OP)
	else if (substr="CAR") then RES(substr, implode(h::tail), OP)
	else if (substr="CDR") then RES(substr, implode(h::tail), OP)
	else if (substr="CONS") then RES(substr, implode(h::tail), OP)
	else if (substr="ATOM") then RES(substr, implode(h::tail), OP)
	else if (substr="IF") then RES(substr, implode(h::tail), OP)
	else if  h = #"$" then RES(substr, implode(h::tail), ID)
    else RES(substr, implode(h::tail), ID);

(************** check_string **************)
fun check_string(substr: string, []: char list): result = RES(substr, "", STR)
| check_string(substr: string, #"\""::tail: char list): result = RES(substr, implode(tail), STR)
| check_string(substr, h::tail): result = check_string(substr^str(h), tail);


(************** check_symbol **************)
fun check_symbol(substr: string, []: char list): result = RES(substr, "", SYM)
| check_symbol(substr: string, h::tail: char list): result =
	if (h = #":" andalso substr=":") then RES(substr^str(h), implode(tail), SYM)
    else RES(substr, implode(h::tail), SYM);


(************** check_substring **************)
fun check_substring(substr: string, []): result = RES("", "", Notoken)
| check_substring(substr: string, h::tail: char list): result =
	if (isdigit(h) orelse (h = #"~")) then check_digit(substr^str(h), tail)
	else if isletter(h) then check_letter(substr^str(h), tail)
	else if issymbol(h) then check_symbol(substr^str(h), tail)
	else if h = #"\"" then check_string("": string, tail: char list)
	else if h = #" " then check_substring("": string, tail: char list) (*caso spazio vuoto*)
	else RES("", "", Notoken); (* caso Notoken *)


(************** stringtobool **************)
(* implementata senza usare quella di libreria *)
fun stringtobools_e(str: string): s_espressione =
	case (str) of
		"T" => T |
		"F" => F |
		_ => T;


(************** stringtoint **************)
(* implementata senza usare quella di libreria *)
fun stringtoint("": string): int = 0
| stringtoint(str: string): int =
	let 
		fun exponent(x,0) =1 | exponent(x,y)= x* exponent(x,y-1)
		val len = size(str)
		val expl = explode(str)
		val head = hd(expl)
		val tail = tl(expl)
	in
		(ord(head)-48) * exponent(10,len-1) + stringtoint(implode(tail))
	end;


(************** lexi **************)
fun lexi("", tkl: token_lexema list): token_lexema list = tkl
| lexi(s: string, tkl: token_lexema list): token_lexema list =
	let
		val group = check_substring("", explode(s))
		val staccum = res1(group);
		val striman = res2(group);
		val tok = res3(group);
	in
		case (tok) of
			LET => lexi(striman, (tkl@[(LET, S(staccum))]): token_lexema list) |
			IN => lexi(striman, (tkl@[(IN, S(staccum))]): token_lexema list) |
			END => lexi(striman, (tkl@[(END, S(staccum))]): token_lexema list) |
			LETREC => lexi(striman, (tkl@[(LETREC, S(staccum))]): token_lexema list) |
			AND => lexi(striman, (tkl@[(AND, S(staccum))]): token_lexema list) |
			LAMBDA => lexi(striman, (tkl@[(LAMBDA, S(staccum))]): token_lexema list) |
			OP => lexi(striman, (tkl@[(OP, S(staccum))]): token_lexema list) |
			ID => lexi(striman, (tkl@[(ID, S(staccum))]): token_lexema list) |
			SYM => lexi(striman, (tkl@[(SYM, S(staccum))]): token_lexema list) |
			(*NM => lexi(striman, (tkl@[(NM, M(NUM(valOf(Int.fromString(staccum)))))]): token_lexema list) |*) (* usando funzioni di libreria *)
			NM => lexi(striman, (tkl@[(NM, M(NUM(stringtoint(staccum))))]): token_lexema list) |
			STR => lexi(striman, (tkl@[(STR, M(STRINGA(staccum)))]): token_lexema list) |
			BOOL => lexi(striman, (tkl@[(BOOL, M(stringtobools_e(staccum)))]): token_lexema list) |		
			Nil => lexi(striman, (tkl@[(Nil, M(NIL))]): token_lexema list) |
			_ => lexi(striman, tkl: token_lexema list) (* caso Notoken *)
	end;


val tkl: token_lexema list = [];



(***************************** SECONDA PARTE *****************************)

(********** GRAMMATICA LISP KIT ********** 
1 Prog::= let Bind in Exp end | letrec Bind in Exp end
2 Bind::= var = Exp X
3 X :: = and Bind | epsilon
4 Exp ::= Prog | var Y |const | List |
          op ( Seq_Exp ) | lambda (Seq_Var ) Exp
5 Y :: = ( Seq_Exp ) | epsilon
6 Seq_Exp::= Exp Z
7 Z::= Seq_Exp | epsilon
8 Seq_Var::= var W
9 W :: = Seq_Var | epsilon
10 List :: = [ Const_List ] | nil
11 Const_List ::= const V | List V
12 V :: = ::Const_list | epsilon *)

(* estrae la constante dal costruttore di M meta_S *)
fun quoting(M(Y)) = Y |
quoting(S(X))= raise e("quoting applicato al costruttore M")
and

(* estrae la stringa dal costruttore di S meta_S *)
unS(S(Y)) = Y |
unS(M(Y))= raise e("estrazione stringa da costruttore M")
and

(* ottiene una lista di stringhe da una lista di Sexpr costruita con Var("stringa") *)
unSexpr([]: Sexpr list): string list = [] |
unSexpr(lst: Sexpr list): string list = unVar(hd(lst))::unSexpr(tl(lst)) |
unSexpr(_)= raise e("estrazione stringa da costruttore M")
and

(* estrae la stringa dal costruttore di Var di Sexpr *)
unVar(Var(Y))=Y |
unVar(_) = raise e("estrazione nome variabile da costruttore errato")
and

(* estrae la stringa dal costruttore di Var di Sexpr *)
uns_espressione(STRINGA(Y))=Y |
uns_espressione(_) = raise e("estrazione nome variabile da costruttore errato")
and

(*testa e un token rappresenta una costante semplice *)
constant(t:token): bool = t=NM orelse t=STR orelse t=Nil orelse t=BOOL
and

(* testa se un token appartiene a FIRST di Exp *)
expfirst(t:token): bool = constant(t) orelse t=LET orelse t=LETREC orelse t=OP orelse t=LAMBDA orelse t=ID
and

(* funzione corrispondente al terminale const *)
const(tkl:token_lexema list): s_espressione * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		case tkhd of
			NM   => (quoting(lxhd),tl(tkl)) | (* numero *)
			STR => (quoting(lxhd),tl(tkl)) | (* stringa *)
			BOOL => (quoting(lxhd),tl(tkl)) | (* T of F *)
			_ => raise e("const applicato a una non-costante")
	end
and

(*funzione corrispondente al terminale var *)
var(tkl:token_lexema list): Sexpr * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if tkhd = ID then (Var(unS(lxhd)),tl(tkl))
		else raise e("non e' una variabile")
	end
and

(*funzione corrispondente al nonterminale Seq_Var *)
seqvar(tkl:token_lexema list): Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if tkhd = ID then (*se e' una variabile*)
		let
			val (sv,tv)=var(tkl) (*prendi primo elemento della sequenza*)
			val (ls,ts)=w(tv) (*riconosci ricorsivamente il resto della sequenza *)
		in
			(sv::ls,ts) (*concatena e restituisci il resto della lista token_lexema*)
        end
		else raise e("la sequenza di variabili non inizia con una variabile")
	end
and

(*funzione ausiliaria di seqvar*)
w(tkl:token_lexema list): Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if tkhd = ID then seqvar(tkl)
		else ([],tkl)
	end
and

(*funzione corrispondente al nonterminale V*)
v(tkl:token_lexema list)=
	if hd(tkl)=(SYM, S("::")) then constlist(tl(tkl)) else (NIL,tkl)
and

(*funzione ausiliaria*)
constlist(tkl:token_lexema list): s_espressione * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (constant(tkhd)) then
			let
				val (sc,tc) = const(tkl)
				val (sv,tv) = v(tc)
			in
				(DOT(sc,sv),tv)
			end
		else
		if ( hd(tkl)=(SYM,S("[")) orelse hd(tkl)=(Nil,M(NIL)) ) then
			let
				val (sl,tl) = lista(tkl)
				val (sv,tv) = v(tl)
			in
				(DOT(sl,sv),tv)
			end
		else raise e("token non compatibile con lista di costanti")
	end
and

(* funzione corrispondente al nonterminale list*)
lista(tkl:token_lexema list): s_espressione * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if tkhd = Nil then (NIL,tl(tkl)) (*lista vuota *)
		else if (tkhd = SYM andalso lxhd=S("["))(* altrimenti inizia con [ *)
		then
			let
				val (cl,tr) = constlist(tl(tkl)) (* riconosco gli elementi della lista *)
			in
				if (hd(tr) <> (SYM,S("]"))) (* deve rimanere ] *)
				then raise e("lista non chiusa correttamente")
				else (cl,tl(tr))
			end
		else raise e("non e' una lista")
	end
and

(* funzione corrispondente al non terminale Seq_Exp *)
seqexp(tkl:token_lexema list): Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (expfirst(tkhd) orelse (tkhd = SYM andalso lxhd=S("[")))
		then (* comincia con una espressione*)
			let
				val (se,te)=exp(tkl)
				val (ls,ts)=z(te)
			in
				(se::ls,ts)
			end
		else raise e("la sequenza di espressioni non inizia con una espressione")
	end
and

(* funzione corrispondente al non terminale Z *)
z(tkl:token_lexema list): Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (expfirst(tkhd) orelse (tkhd = SYM andalso lxhd=S("[")))
		then seqexp(tkl)
		else ([],tkl)
	end
and

(* funzione corrispondente al non terminale Bind *)
bind(tkl:token_lexema list): string list * Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if(tkhd = ID) then
			let
				val sr = unS(lxhd); (* prima variabile *)
			in
				if hd(tl(tkl)) = (SYM, S("=")) then
				let
					val (er,ter) = exp(tl(tl(tkl))) (* espressione corrispondente alla prima variabile *)
					val (fw1,fw2,tf) = x(ter) (*riconosci il resto del bind*)
				in
					(sr::fw1,er::fw2,tf)
				end
				else raise e("variabile non inizializzata con =")
			end
		else raise e("il bind non comicia con un identificatore")
	end
and

(* funzione corrispondente al non terminale X *)
x(tkl:token_lexema list): string list * Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
 		case tkhd of
			AND => bind(tl(tkl)) |
			_   => ([],[],tkl)
	end
and

(* funzione corrispondente al non terminale Y *)
y(tkl:token_lexema list): Sexpr list * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (hd(tkl) = (SYM,S("("))) then
			let
				val (sv,tv) = seqexp(tl(tkl))
			in
				if (hd(tv) <> (SYM,S(")")))
				then raise e("seqexp non chiusa correttamente")
				else (sv,tl(tv))
			end
		else ([],tkl)	
	end
and

(* funzione corrispondente al nonterminale Exp :   DA FARE *)
exp(tkl:token_lexema list): Sexpr * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (tkhd=NM) then (Quote(quoting(lxhd)),tl(tkl))
		else if (tkhd=STR) then (Quote(quoting(lxhd)),tl(tkl))
		else if (tkhd=BOOL) then (Quote(quoting(lxhd)),tl(tkl))
		else if (tkhd=Nil) then (Quote(quoting(lxhd)),tl(tkl))
		else if (lxhd=S("[")) then
			let
				val (se,tk) = lista(tkl)
			in
				(Quote(se),tk)
			end
		else if (tkhd=ID andalso hd(tl(tkl))=(SYM, S("("))) then
			let
				val (a,b) = seqexp(tl(tl(tkl)))
			in
				if (hd(b)) = (SYM,S(")")) then
				(Call(Var(unS(lxhd)), a), tl(b))
				else 
				raise e("Sexpr non chiusa correttamente con )")
			end
		else if (tkhd=ID) then var(tkl)
		else if (hd(tkl)=(OP,S("IF")) andalso hd(tl(tkl))=(SYM, S("("))) then
			let
				val (s1,tk1) = exp(tl(tl(tkl)))
				val (s2,tk2) = exp(tk1)
				val (s3,tk3) = exp(tk2)
			in
				if hd(tk3)=(SYM,S(")")) then
				(If(s1,s2,s3), tl(tk3))
				else raise e("espressione IF non chiusa corretamente con )")
			end
		else if (tkhd=OP andalso hd(tl(tkl))=(SYM,S("("))) then
			let
				val (sv,tv) = seqexp(tl(tl(tkl)))
			in
				if (hd(tv) <> (SYM,S(")")))
				then raise e("seqexp non chiusa correttamente con )")
				else (Op(unS(lxhd),sv), tl(tv))
			end

		else if (tkhd=LAMBDA andalso hd(tl(tkl))=(SYM,S("("))) then
			let
				val (sv,tv) = seqvar(tl(tl(tkl)))
			in
				if (hd(tv) <> (SYM,S(")")))
				then raise e("seqexp non chiusa correttamente con )")
				else
					let
						val (a,b) = exp(tl(tv))
					in
						(Lambda(unSexpr(sv),a), b)
					end
			end
		else if ((tkhd = LET) orelse (tkhd = LETREC)) then prog(tkl)
		else raise e("exp con nessuna produzione consentita")
	end
and

(* funzione corrispondente al nonterminale Prog: DA FARE *)
prog(tkl:token_lexema list): Sexpr * token_lexema list =
	let
		val tkhd = #1(hd(tkl))
		val lxhd = #2(hd(tkl))
	in
		if (tkhd = LET) then
			let
				val (sv,tv,rs) = bind(tl(tkl))
			in
				if (hd(rs))=(IN,S("in")) then
					let
						val (a,b) = exp(tl(rs))	
					in
						if (hd(b))=(END,S("end")) andalso hd(tl(b))=(SYM,S("$")) then (Let(a,sv,tv), [])
						else if (hd(b))=(END,S("end")) then (Let(a,sv,tv), tl(b))
						else raise e("produzione [Prog::= let Bind in Exp end] non chiusa con end")
					end
				else raise e("in non presente nella produzione [Prog::= let Bind in Exp end]")
			end		
		else if (tkhd = LETREC) then
			let
				val (sv,tv,rs) = bind(tl(tkl))
			in
				if (hd(rs))=(IN,S("in")) then
					let
						val (a,b) = exp(tl(rs))	
					in
						if (hd(b))=(END,S("end")) andalso hd(tl(b))=(SYM,S("$")) then (Letrec(a,sv,tv), [])
						else if (hd(b))=(END,S("end")) then (Letrec(a,sv,tv), tl(b))
						else raise e("produzione [Prog::= letrec Bind in Exp end] non chiusa con end")
					end
				else raise e("in non presente nella produzione [Prog::= letrec Bind in Exp end]")
			end
		else raise e("produzione Prog non inizia con let/letrec")
	end

(* stringhe di esempio del progetto *)
val st1 = "let x = 5 and y = 6 in IF ( LEQ ( x y ) SUB ( y x ) SUB ( x y ) ) end $"
val st2 = " let N = 3 and L = LAMBDA ( P Q R ) DIV (ADD ( ADD ( MUL ( P P ) MUL ( Q Q ) ) MUL ( R R ) ) N ) in L ( 2 4 6 ) end $"
val st3 = " letrec FACT = LAMBDA ( X ) IF ( EQ ( X 0 ) 1 MUL ( X FACT ( SUB ( X 1 ) ) ) ) and G = LAMBDA ( H L ) IF ( EQ ( L NIL ) L CONS ( H ( CAR ( L ) ) G ( H CDR ( L ) ) ) ) in G ( FACT [2::3::4::5] ) end $";

val lexer1 = lexi(st1, tkl);
val parser1 = prog(lexer1);
val lexer2 = lexi(st2, tkl);
val parser2 = prog(lexer2);
val lexer3 = lexi(st3, tkl);
val parser3 = prog(lexer3);
