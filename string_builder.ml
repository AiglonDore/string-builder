type string_builder =
  |Feuille of string * int 
  |Noeud of string_builder * int * string_builder
;;

let size strb = match strb with
  |Feuille(_,n)->n
  |Noeud(_,n,_)->n
;;
let rec check sb = match sb with
  |Feuille(s,n)-> String.length s = n
  |Noeud(l,n,r)-> n = (size l + size r) && check l && check r
;;

let word str = Feuille(str,String.length str);;

let concat text1 text2=
  if size text1 = 0 then
    text2
  else if size text2 = 0 then
    text1
  else
    Noeud(text1,(size text1)+(size text2),text2)
;;

let rec char_at i strb=match strb with
  |Feuille(s,_)->String.get s i
  |Noeud(left,_,right)->begin
      if size left > i then
        char_at i left
      else
        char_at (i-size left) right
    end
;;

(*i must be between size sb and 0 and i+m too*)
let rec sub_string i m sb=if m = 0 then word ""
                          else
                            begin
                              match sb with
                              |Feuille(w,_)->Feuille(String.sub w i m,m)
                              |Noeud(sbl,n,sbr)->begin
                                  if i - size sbl = 0 && size sbr = m then
                                    sbr
                                  else if m = size sbl && i - m = 0 then
                                    sbl
                                  else if i >= (size sbl) then
                                    sub_string (i-size sbl) m sbr
                                  else if (size sbl) > i+m then
                                    sub_string i m sbl
                                  else
                                    concat (sub_string i (size sbl - i) sbl) (sub_string 0 (m- (size sbl - i)) sbr)
                                end
                            end
;;

let cost strb=
  let rec aux acc prof tree=match tree with
    |Feuille(_,n)->acc + (prof * n)
    |Noeud(left,n,right)->let cost_line = aux 0 (prof+1) left + aux 0 (prof+1) right
                          in
                          acc + cost_line
  in
  aux 0 0 strb
;;

Random.self_init ();;
let random_string i=
  let generate_random_string () = 
    let n = Random.int 1000 in
    let rec aux acc i=if i = 0 then acc else aux (acc ^ (Char.escaped (Char.chr ((Random.int 26)+97)))) (i-1)
    in
    aux "" n
  in
  if i = 0 then
    word (generate_random_string ())
  else
    let rec aux tree j=
      if j<0 then
        tree
      else
        let x = generate_random_string ()
        in
        aux (concat tree (word x)) (j-1)
    in
    aux (word "") i
;;

let rec depth strb = match strb with
  |Feuille(_,_)->0
  |Noeud(left,_,right)->1 + max (depth left) (depth right)
;;

let rec list_of_string strb=match strb with
  |Feuille(s,_)->[s]
  |Noeud(left,_,right)->(list_of_string left)@(list_of_string right)
;;

let rec list_of_leaf strb=match strb with
  |Feuille(s,n)->[Feuille(s,n)]
  |Noeud(left,_,right)->(list_of_leaf left) @ (list_of_leaf right)
;;

let balance sb=
  let sb_list = list_of_leaf sb in
  let rec trouve_succ acc p=match p with
    |[]->acc
    |x::y::q->begin
        if cost (concat x y) < cost (concat (fst acc) (snd acc)) then
          trouve_succ (x,y) (y::q)
        else
          trouve_succ acc (y::q)
      end
    |[_]->acc
  in
  let rec replace_min p (x,y)=match p with
    |[]->[]
    |[a]->[a]
    |a::b::q->if a=x && b=y then (concat x y)::q else replace_min (b::q) (x,y)
  in
  let rec aux l=match l with
    |[]->Feuille("",0)
    |[x]->x
    |x::y::q->begin
        let min_cost = trouve_succ (x,y) (x::y::q) in
        aux (replace_min l min_cost)
      end
  in aux sb_list    
;;
