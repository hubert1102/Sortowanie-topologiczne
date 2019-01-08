(* Zadanie: Sortowanie topologiczne *)
(* Autor: Hubert Budzynski *)
(* Recenzent: Maciej Procyk *)

open PMap

exception Cykliczne


(* jezeli jakis wierzcholek nie ma przypisanego stanu (nie znajduje sie w mapie stanow wierzcholkow), to go nie odwiedzilismy,
   jezeli jest Visited, to odwiedzilismy juz go, ale nie dodalismy jeszcze do listy wynikowej,
   jezeli jest Added, to zostal juz dodany do listy wynikowej *)
type vertex_status =
  | Visited
  | Added


(* make_graph l zwraca mape m, gdzie kluczami sa wierzcholki w grafie i sa im przypisane listy sasiedztwa *)
(* w mapie koncowej kluczami sa tylko wierzcholki, dla ktorych istnieje lista sasiedztwa w l *)
let make_graph l =
  let rec pom l m = match l with
    | [] -> m
    | (a,l)::t -> pom t (add a l m)
  in pom l empty


(* topol l zwraca liste posortowanych topologicznie wierzcholkow dla danej listy par sasiedztwa wierzcholkow *)
let topol l =
  let graph = make_graph l in
  let vis = ref empty in (* vis to mapa, ktora dla wiercholkow mowi, jaki maja status (Visited, Added, czy zaden z nich) *)
  let wyn = ref [] in (* lista wynikowa *)
  (* dfs v lv wykonuje algorytm DFS dla wierzcholka v, gdzie lv to lista nierozpatrzonych sasiadow v *)
  let rec dfs vertex lv =
    match lv with
    | [] -> (
      (*jezeli nie mozemy isc dalej z jakiegos wierzcholka, to dodajemy go do listy wynikowej *)
      vis := add vertex Added !vis;
      wyn := vertex::(!wyn);
    )
    | h::t ->
       if mem h !vis then
         match find h !vis with
         (* jezeli wierzchalek h byl rozpatrywany, ale nie dodany, to w grafie istnieje cykl *)
         (* jezeli h byl juz dodany, to wykonuje DFS dla wierzcholka vertex z lista pomniejszana o h *)
         (* poniewaz h nie musze juz rozpatrywac *)
         | Visited -> raise Cykliczne
         | Added -> dfs vertex t
       else
         (
           (*jezeli w h nie bylem wczesniej, to oznaczam go jako odwiedzony *)
           vis := add h Visited !vis;
           if mem h graph then
             (*jezeli da sie gdzies pojsc z h, to odpalam DFS na nim *)
             dfs h (find h graph)
           else
             (
               (*w przeciwnym wypadku dodaje h do wyniku *)
               vis := add h Added !vis;
               wyn := h::(!wyn);
             );
           (* rozpatrzylem h, wiec ide do kolejnego sasiada vertex *)
           dfs vertex t;
         )
  in
  let rec top_sort l =
    (* wykonuje dfs dla kazdego wierzcholka w grafie i zwracam liste wynikowa *)
    match l with
    | [] -> !wyn
    | (h, lh)::t -> if mem h !vis then top_sort t
                    else (
                      dfs h lh;
                      top_sort t;
                    )
  in top_sort l
  ;;
