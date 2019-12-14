exception Invalid_Input_exception

val B = 10000;
val M = 4;

(* remove zeros at starting of a string *)
fun remove_zeros_string s = 
	if size(s) < 2 then s	
	else 
		let val hd = str (String.sub(s, 0));
				val tl = String.extract(s,1,NONE);
		in 
			if valOf(Int.fromString(hd)) <> 0 then s
			else remove_zeros_string tl
		end;

(* make a string which has zeros n times *)
fun make_string_zeros n = 
	if n <= 0 then []
	else if n = 1 then ["0"]
	else "0"::make_string_zeros(n-1);

(* adds n number zeros at the starting of a string s *)
fun add_zeros_string s n =
	if n<1 then s 
	else 
		let val zeros = make_string_zeros n
		in String.concat ( zeros@[s])
		end;

(*convert String to list of ints*)
fun fromString st =
	if List.all Char.isDigit (String.explode st) then
		let val s = remove_zeros_string st;
		in
			let 
				val n = size(s)
				fun iter(ss, ls, i, x, y) =
					if (y-x) < 4 then valOf(Int.fromString(String.substring(ss,x,y-x+1)))::ls
					else if size(ss)<5 then valOf(Int.fromString(ss))::ls
					else iter(ss, valOf(Int.fromString (String.substring(ss, i, 4)))::ls, i-4, x, y-4);
			in 
				if n < 5 then [valOf(Int.fromString(s))]
				else iter(s, [], n-4, 0, n-1)
			end
		end
	else raise Invalid_Input_exception;

(* convert a list of integers to string, the output string may have leading zeros *)
fun toStringUtil [] = ""
	| toStringUtil (x::xs) =
	let 
		val s = Int.toString (x);
		val n = 4 - size(s);
	in 
		let val toappend = add_zeros_string s n;
		in 
			if n > 0 then String.concat(toappend :: [toStringUtil(xs)])
			else String.concat(s :: [toStringUtil(xs)])
		end
	end;

(* converts list of ints to string and removes leading zeros *)
fun toString s = 
	remove_zeros_string (toStringUtil s);


(* Add leading Zeros to list *)
fun add_zeros num n =
	let 
		fun makelist n = 
		if n = 0 then []
		else if n = 1 then [0]
		else 0::makelist(n-1);
	in if n<1 then num else makelist n @ num 
	end;

(* Remove leading Zeros from list *)
fun remove_zeros [] = []
	| remove_zeros [0] = [0]
	| remove_zeros (x::xs) = if x=0 then remove_zeros(xs) else x::xs;

(* make a list x to have size n by adding leading zeros *)
fun make_equal x n = 
	let val nx = n - length x;
	in 
		add_zeros x nx
	end;

(* add numbers in two lists *)
fun add m n = 
	let 
		val mnew = make_equal m (length n) ;
		val nnew = make_equal n (length m) ;
		fun addutil ([], [], a, c) = 
			if c > 0 then c::a else a
			| addutil (x::xs, y::ys, a, c) = 
			let val sum = x+y+c;
			in addutil (xs, ys, (sum mod B)::a, sum div B)
			end;
	in
		addutil (rev mnew, rev nnew, [], 0)
	end;


(*multiply a list of ints with a number ; a is list and b is int*)
fun mult (x, b) = 
	let 
		fun mult (x::[], b) = [x*b]
			| mult (x::xs, b) =
				let 
					val mult_xs = (mult (xs,b));
				in 
					if (List.nth (mult_xs,0)) > (B-1) then (List.nth(mult_xs,0) div B + x*b) :: (List.nth (mult_xs,0) mod B) :: List.tl mult_xs
					else x*b :: mult_xs
				end;
	in  
		let val (y::ys) = mult(x,b)
		in 
			if y > (B-1) then y div B :: y mod B :: ys
			else y::ys
		end
	end;


(* multiply two numbers that are in the form of list of ints *)
fun multiply [] [] = []
	| multiply x (y::[]) = mult (x,y) 
	| multiply x (y::ys) =
		let 
			val prod = rev (mult (x, y));
			val n = length ys;
		in
			let 
				val prod_1 = rev (add_zeros prod n);
				val prod_2 = multiply x ys;
			in 
				add prod_1 prod_2
			end
		end;

(* subtraction of two numbers , returns reversed order *)
fun swb b []      []  = if b=0 then [] else [~b]
  | swb b l       []  = swb b l [0]
  | swb b []      r   = swb b [0] r
  | swb b (l::ls) (r::rs) =
    let
        val digit = l - b - r;
    in
        if digit >= 0 then
            digit :: (swb 0 ls rs)
        else
            (digit + B) :: (swb 1 ls rs)
    end;

(* subtract 1 from a number in the list form *)
fun subtract1 num_list = 
	let val ls = rev num_list
	in remove_zeros (rev (swb 0 ls [1]))
	end;


(* subtract two numbers in the list form and remove leading zeros if any *)
fun subtract_ints n1 n2 = 
	let 
		val nn1 = rev n1;
		val nn2 = rev n2;
	in 
		let val diff = rev (swb 0 nn1 nn2)
		in remove_zeros diff
		end
	end;


(* get first n-m elements of list *)
fun get_x1 ls m =
	let val n = length ls;
	in 
		if length ls > m then List.take (ls, n-m)
		else []
	end; 


(* get last m elements of list *)
fun get_x0 ls m = 
	let val n = length ls
	in 
		if n > m then rev (List.take ( rev ls , m))
		else ls
	end;

(* karatsuba for multiplication *)
fun karatsuba [] [] = []
	| karatsuba [x] [y] = multiply [x] [y]
	| karatsuba nn1 nn2 =
		let 
			val n1 = make_equal nn1 (length nn2) ;
			val n2 = make_equal nn2 (length nn1) ;
		in
			let val m = (length n1 ) div 2;
			in
				let
					val x1 = get_x1 n1 (m);
					val x0 = get_x0 n1 (m);
					val y1 = get_x1 n2 (m);
					val y0 = get_x0 n2 (m);
				in 
					
					let 
						val z0 = karatsuba x0 y0;
						val z2 = karatsuba x1 y1;
						val x0_x1 = add x0 x1;
						val y0_y1 = add y0 y1;
						val z1_1 = karatsuba x0_x1 y0_y1;
						val z1_2 = add z0 z2;
					in 
						let val z1 = subtract_ints z1_1 z1_2 
						in 
							(*z1_1*)
							let 
								val z2_2m = rev (add_zeros (rev z2) (2*m))
								val z1_m = rev (add_zeros (rev z1) m)
								val z1_z2 = add z2_2m z1_m
							in 
								remove_zeros (add z1_z2 z0)
							end
						end
					end
				end
			end
		end;

(* utility function for factorial *)
fun fact [] = [1]
	| fact [0] = [1]
	| fact [1] = [1]
	| fact n =
	karatsuba (fact (subtract1 n)) n; 

(* Main factorial function *)
fun factorial n_str_list = 
	let val n = remove_zeros (fromString n_str_list);
	in 
		remove_zeros_string (toString (fact n))
	end;
