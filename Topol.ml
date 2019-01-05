Open List;;
Open PMap;;

exception Cykliczne;;

type vertex_status = 
	| visited
	| added
;;

let make_graph l = 
	let rec pom l m = match l with
		| [] -> m
		| (a,l)::t -> pom t (add a l m)
	in l empty
;;

let topol l = 
	let graph = make_graph l in
	let vis = ref empty in
	let wyn = ref [] in
	let rec dfs vertex =
		let l = find vertex graph in
		let iter l lw = match l with
			| [] -> vertex::lw
			| h::t -> 
				if mem h vis then match find h vis with
					| visited -> raise Cykliczne
					| added -> iter t lw
				else dfs h
		in iter l wyn
	in let policz l =
		match l with
		| [] -> wyn
		| (x, l)::t -> begin
			wyn := dfs x;
			policz t
		end
	in policz l
;;
