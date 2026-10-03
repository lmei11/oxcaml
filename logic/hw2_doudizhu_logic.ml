(* ------------------------------------------------------------------ *)
(* Dou Dizhu (斗地主) - core state representation + move application  *)
(*                                                                      *)
(* Scope: cards, hands, players/roles, table state, and applying moves *)


(* ---------------------------- Rank ---------------------------------- *)

module Rank = struct
  type t =
    | Three | Four | Five | Six | Seven | Eight | Nine | Ten
    | Jack | Queen | King | Ace | Two
    | SmallJoker | BigJoker

  (* Dou Dizhu ordering: 3 < 4 < ... < A < 2 < SmallJoker < BigJoker *)
  let to_int = function
    | Three -> 3 | Four -> 4 | Five -> 5 | Six -> 6 | Seven -> 7
    | Eight -> 8 | Nine -> 9 | Ten -> 10 | Jack -> 11 | Queen -> 12
    | King -> 13 | Ace -> 14 | Two -> 15
    | SmallJoker -> 16 | BigJoker -> 17

  let compare a b = compare (to_int a) (to_int b)
  let equal a b = to_int a = to_int b

  (* Ranks that can appear in a straight / consecutive sequence.
     2 and the jokers are excluded, per standard rules. *)
  let straightable =
    [ Three; Four; Five; Six; Seven; Eight; Nine; Ten;
      Jack; Queen; King; Ace ]

  let is_straightable r = List.exists (equal r) straightable

  let to_string = function
    | Three -> "3" | Four -> "4" | Five -> "5" | Six -> "6"
    | Seven -> "7" | Eight -> "8" | Nine -> "9" | Ten -> "10"
    | Jack -> "J" | Queen -> "Q" | King -> "K" | Ace -> "A" | Two -> "2"
    | SmallJoker -> "sJ" | BigJoker -> "BJ"
end

(* ---------------------------- Suit ----------------------------------- *)

module Suit = struct
  type t = Spade | Heart | Club | Diamond

  let all = [ Spade; Heart; Club; Diamond ]

  let to_string = function
    | Spade -> "S" | Heart -> "H" | Club -> "C" | Diamond -> "D"
end

(* ---------------------------- Card ------------------------------------ *)

module Card = struct
  (* Jokers carry no suit - their distinct value (small vs big) is already
     captured by having two separate Rank constructors. *)
  type t = { rank : Rank.t; suit : Suit.t option }

  let make rank suit = { rank; suit }
  let joker_small = { rank = Rank.SmallJoker; suit = None }
  let joker_big = { rank = Rank.BigJoker; suit = None }

  let compare a b = Rank.compare a.rank b.rank

  let to_string t =
    match t.suit with
    | None -> Rank.to_string t.rank
    | Some s -> Rank.to_string t.rank ^ Suit.to_string s
end

(* ---------------------------- Deck ------------------------------------- *)

module Deck = struct
  (* the 13 ranks that appear as normal (suited) cards *)
  let normal_ranks = Rank.straightable @ [ Rank.Two ]

  let full () : Card.t list =
    let suited =
      List.concat_map
        (fun suit -> List.map (fun rank -> Card.make rank (Some suit)) normal_ranks)
        Suit.all
    in
    suited @ [ Card.joker_small; Card.joker_big ]

  let shuffle (deck : Card.t list) : Card.t list =
    let arr = Array.of_list deck in
    let n = Array.length arr in
    for i = n - 1 downto 1 do
      let j = Random.int (i + 1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    Array.to_list arr

  (* Deals a 54-card deck into three 17-card hands plus a 3-card kitty. *)
  let deal (deck : Card.t list) : Card.t list * Card.t list * Card.t list * Card.t list =
    if List.length deck <> 54 then invalid_arg "Deck.deal: expected 54 cards";
    let rec take n lst =
      if n = 0 then ([], lst)
      else
        match lst with
        | [] -> invalid_arg "Deck.deal: ran out of cards"
        | x :: rest ->
          let taken, remaining = take (n - 1) rest in
          (x :: taken, remaining)
    in
    let h0, rest = take 17 deck in
    let h1, rest = take 17 rest in
    let h2, rest = take 17 rest in
    (h0, h1, h2, rest (* kitty, 3 cards *))
end

(* ---------------------------- Roles / Players --------------------------- *)

module Role = struct
  type t = Landlord | Peasant

  let to_string = function
    | Landlord -> "Landlord"
    | Peasant -> "Peasant"
end

module Player = struct
  type t = {
    id : int; (* 0, 1, 2 *)
    hand : Card.t list;
    role : Role.t;
  }

  let sorted_hand p = List.sort Card.compare p.hand

  let to_string p =
    Printf.sprintf "Player %d (%s): %s" p.id
      (Role.to_string p.role)
      (String.concat " " (List.map Card.to_string (sorted_hand p)))
end

(* ---------------------------- Combos ------------------------------------ *)

module Combo = struct
  (* Every variant carries the full list of cards that make up the play,
     so we always know exactly what was placed on the table. *)
  type t =
    | Single of Card.t list         (* len 1 *)
    | Pair of Card.t list           (* len 2, same rank *)
    | Triple of Card.t list         (* len 3, same rank *)
    | TripleSingle of Card.t list   (* len 4: triple + 1 unrelated single *)
    | TriplePair of Card.t list     (* len 5: triple + 1 unrelated pair *)
    | Straight of Card.t list       (* len >=5, consecutive singles *)
    | DoubleStraight of Card.t list (* len >=6 (>=3 pairs), consecutive pairs *)
    | Airplane of Card.t list       (* consecutive triples, no wings *)
    | AirplaneSingle of Card.t list (* consecutive triples + single wings *)
    | AirplanePair of Card.t list   (* consecutive triples + pair wings *)
    | Bomb of Card.t list           (* len 4, same rank *)
    | Rocket of Card.t list         (* both jokers *)

  let cards_of = function
    | Single c | Pair c | Triple c | TripleSingle c | TriplePair c
    | Straight c | DoubleStraight c | Airplane c | AirplaneSingle c
    | AirplanePair c | Bomb c | Rocket c -> c

  (* --- helpers for classification --- *)

  (* (rank, how many cards of that rank), sorted ascending by rank *)
  let rank_counts (cards : Card.t list) : (Rank.t * int) list =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (c : Card.t) ->
        let cur = try Hashtbl.find tbl c.rank with Not_found -> 0 in
        Hashtbl.replace tbl c.rank (cur + 1))
      cards;
    Hashtbl.fold (fun r c acc -> (r, c) :: acc) tbl []
    |> List.sort (fun (r1, _) (r2, _) -> Rank.compare r1 r2)

  let is_consecutive (ranks : Rank.t list) : bool =
    match List.sort Rank.compare ranks with
    | [] | [ _ ] -> true
    | r0 :: rest ->
      let _, ok =
        List.fold_left
          (fun (prev, ok) r -> (r, ok && Rank.to_int r = Rank.to_int prev + 1))
          (r0, true) rest
      in
      ok

  let is_rocket (cards : Card.t list) : bool =
    match List.sort Rank.compare (List.map (fun (c : Card.t) -> c.rank) cards) with
    | [ Rank.SmallJoker; Rank.BigJoker ] -> true
    | _ -> false

  (* ranks that occur exactly [n] times, sorted ascending *)
  let ranks_with_count counts n =
    counts |> List.filter (fun (_, c) -> c = n) |> List.map fst

  (* Try to classify an arbitrary list of cards as a legal combo.
     Returns None if the cards don't form any recognized, legal shape. *)
  let classify (cards : Card.t list) : t option =
    let n = List.length cards in
    if n = 0 then None
    else if is_rocket cards then Some (Rocket cards)
    else
      let counts = rank_counts cards in
      let ranks = List.map fst counts in
      match n, counts with
      | 1, [ (_, 1) ] -> Some (Single cards)
      | 2, [ (_, 2) ] -> Some (Pair cards)
      | 3, [ (_, 3) ] -> Some (Triple cards)
      | 4, [ (_, 4) ] -> Some (Bomb cards)
      | 4, [ (_, a); (_, b) ] when (a = 3 && b = 1) || (a = 1 && b = 3) ->
        Some (TripleSingle cards)
      | 5, [ (_, a); (_, b) ] when (a = 3 && b = 2) || (a = 2 && b = 3) ->
        Some (TriplePair cards)
      | _ when n >= 5 && List.for_all (fun (_, c) -> c = 1) counts
               && List.for_all Rank.is_straightable ranks
               && is_consecutive ranks ->
        Some (Straight cards)
      | _ when n >= 6 && n mod 2 = 0
               && List.for_all (fun (_, c) -> c = 2) counts
               && List.length counts >= 3
               && List.for_all Rank.is_straightable ranks
               && is_consecutive ranks ->
        Some (DoubleStraight cards)
      | _ ->
        (* Airplane family: some number (>=2) of consecutive triples,
           optionally with single or pair "wings" attached. *)
        let triple_ranks =
          ranks_with_count counts 3 |> List.filter Rank.is_straightable
        in
        let num_triples = List.length triple_ranks in
        if num_triples >= 2 && is_consecutive triple_ranks
           && num_triples * 3 <= n
        then begin
          let wing_count = n - (num_triples * 3) in
          if wing_count = 0 then Some (Airplane cards)
          else if wing_count = num_triples then
            (* wings must all be singles: every non-triple rank has count 1 *)
            let non_triple_counts =
              List.filter (fun (r, _) -> not (List.mem r triple_ranks)) counts
            in
            if List.for_all (fun (_, c) -> c = 1) non_triple_counts then
              Some (AirplaneSingle cards)
            else None
          else if wing_count = num_triples * 2 then
            let non_triple_counts =
              List.filter (fun (r, _) -> not (List.mem r triple_ranks)) counts
            in
            if List.for_all (fun (_, c) -> c = 2) non_triple_counts then
              Some (AirplanePair cards)
            else None
          else None
        end
        else None

  (* The rank that determines strength for same-shape comparisons. *)
  let dominant_rank_int (combo : t) : int =
    let cards = cards_of combo in
    let counts = rank_counts cards in
    let max_rank rs = List.fold_left (fun m r -> max m (Rank.to_int r)) 0 rs in
    match combo with
    | Single _ | Pair _ | Triple _ | Bomb _ -> Rank.to_int (List.hd cards).rank
    | TripleSingle _ | TriplePair _ ->
      Rank.to_int (List.hd (ranks_with_count counts 3))
    | Straight _ | DoubleStraight _ -> max_rank (List.map fst counts)
    | Airplane _ | AirplaneSingle _ | AirplanePair _ ->
      max_rank (ranks_with_count counts 3)
    | Rocket _ -> max_int (* never actually compared: Rocket always wins *)

  (* Two combos must have the same "shape" (type + length) before their
     dominant ranks can be compared. *)
  let same_shape (a : t) (b : t) : bool =
    match a, b with
    | Single _, Single _
    | Pair _, Pair _
    | Triple _, Triple _
    | TripleSingle _, TripleSingle _
    | TriplePair _, TriplePair _
    | Bomb _, Bomb _
    | Rocket _, Rocket _ -> true
    | Straight c1, Straight c2 -> List.length c1 = List.length c2
    | DoubleStraight c1, DoubleStraight c2 -> List.length c1 = List.length c2
    | Airplane c1, Airplane c2 -> List.length c1 = List.length c2
    | AirplaneSingle c1, AirplaneSingle c2 -> List.length c1 = List.length c2
    | AirplanePair c1, AirplanePair c2 -> List.length c1 = List.length c2
    | _ -> false

  (* Does [challenger] beat [incumbent] currently on the table? *)
  let beats ~(incumbent : t) (challenger : t) : bool =
    match challenger, incumbent with
    | Rocket _, Rocket _ -> false (* can't happen - only one rocket exists *)
    | Rocket _, _ -> true
    | _, Rocket _ -> false
    | Bomb b1, Bomb b2 -> dominant_rank_int (Bomb b1) > dominant_rank_int (Bomb b2)
    | Bomb _, _ -> true
    | _, Bomb _ -> false
    | _, _ ->
      same_shape challenger incumbent
      && dominant_rank_int challenger > dominant_rank_int incumbent

  let to_string (combo : t) : string =
    let cards = cards_of combo in
    let card_str = String.concat " " (List.map Card.to_string (List.sort Card.compare cards)) in
    let kind =
      match combo with
      | Single _ -> "Single" | Pair _ -> "Pair" | Triple _ -> "Triple"
      | TripleSingle _ -> "Triple+Single" | TriplePair _ -> "Triple+Pair"
      | Straight _ -> "Straight" | DoubleStraight _ -> "DoubleStraight"
      | Airplane _ -> "Airplane" | AirplaneSingle _ -> "Airplane+Singles"
      | AirplanePair _ -> "Airplane+Pairs" | Bomb _ -> "Bomb" | Rocket _ -> "Rocket"
    in
    Printf.sprintf "%s [%s]" kind card_str
end

(* ---------------------------- Game state --------------------------------- *)

module Game = struct
  type table_state = {
    combo : Combo.t;
    played_by : int; (* player id *)
  }

  type t = {
    players : Player.t array; (* indices 0,1,2 *)
    turn : int;                (* whose turn it is now *)
    table : table_state option; (* None = fresh trick, anyone may lead *)
    passes : int;               (* consecutive passes since the last real play *)
    winner : int option;
  }

  (* No bidding: player 0 is always dealt the landlord role + the kitty. *)
  let init () : t =
    let deck = Deck.shuffle (Deck.full ()) in
    let h0, h1, h2, kitty = Deck.deal deck in
    let players =
      [| { Player.id = 0; hand = h0 @ kitty; role = Role.Landlord };
         { Player.id = 1; hand = h1; role = Role.Peasant };
         { Player.id = 2; hand = h2; role = Role.Peasant } |]
    in
    { players; turn = 0; table = None; passes = 0; winner = None }

  let current_player (g : t) : Player.t = g.players.(g.turn)

  let next_turn i = (i + 1) mod 3

  let to_string (g : t) : string =
    let players_str =
      g.players |> Array.to_list |> List.map Player.to_string |> String.concat "\n"
    in
    let table_str =
      match g.table with
      | None -> "(empty - new trick, anyone may lead)"
      | Some { combo; played_by } ->
        Printf.sprintf "%s (played by Player %d)" (Combo.to_string combo) played_by
    in
    let winner_str =
      match g.winner with
      | None -> "in progress"
      | Some id -> Printf.sprintf "Player %d wins!" id
    in
    Printf.sprintf "%s\nTurn: Player %d | Passes in a row: %d\nTable: %s\nStatus: %s"
      players_str g.turn g.passes table_str winner_str

  (* Removes [cards] from [hand] (as a multiset). None if any card in
     [cards] isn't actually held. *)
  let remove_cards (hand : Card.t list) (cards : Card.t list) : Card.t list option =
    let rec remove_one target = function
      | [] -> None
      | x :: rest ->
        if x = target then Some rest
        else
          match remove_one target rest with
          | None -> None
          | Some rest' -> Some (x :: rest')
    in
    List.fold_left
      (fun acc c -> match acc with None -> None | Some h -> remove_one c h)
      (Some hand) cards
end

(* ---------------------------- Moves --------------------------------------- *)

module Move = struct
  type t =
    | Play of Card.t list
    | Pass

  (* Apply a move to a game state, returning the new state or an error
     describing why the move is illegal. *)
  let apply (g : Game.t) (move : t) : (Game.t, string) result =
    if g.winner <> None then Error "the game is already over"
    else
      match move with
      | Pass -> (
        match g.table with
        | None -> Error "cannot pass: you must lead a new trick"
        | Some { played_by; _ } ->
          let passes = g.passes + 1 in
          if passes >= 2 then
            (* both opponents have passed: trick over, winner leads again *)
            Ok { g with turn = played_by; table = None; passes = 0 }
          else Ok { g with turn = Game.next_turn g.turn; passes })
      | Play cards -> (
        let player = Game.current_player g in
        match Game.remove_cards player.hand cards with
        | None -> Error "you don't hold those cards"
        | Some new_hand -> (
          match Combo.classify cards with
          | None -> Error "that is not a legal combination of cards"
          | Some combo ->
            let legal =
              match g.table with
              | None -> true (* leading a fresh trick: any legal combo goes *)
              | Some { combo = incumbent; _ } -> Combo.beats ~incumbent combo
            in
            if not legal then Error "that play doesn't beat the current combo"
            else
              let updated_player = { player with Player.hand = new_hand } in
              let players = Array.copy g.players in
              players.(g.turn) <- updated_player;
              let winner = if new_hand = [] then Some g.turn else None in
              Ok
                { Game.players;
                  table = Some { Game.combo; played_by = g.turn };
                  passes = 0;
                  turn = Game.next_turn g.turn;
                  winner
                }))
end

(* ------------------------------------------------------------------ *)
(* Demo: a scripted sequence of moves showing the state transitions   *)
(* actually work, including the everyone-passes -> trick-reset rule.  *)
(* ------------------------------------------------------------------ *)

(* Small demo: not a REPL, just a scripted sequence of moves showing the
   state transitions work, including the "everyone passes -> trick resets
   to the winner" rule. *)

let show label g =
  Printf.printf "=== %s ===\n%s\n\n" label (Game.to_string g)

let apply_and_show label g move =
  match Move.apply g move with
  | Ok g' ->
    show label g';
    g'
  | Error msg ->
    Printf.printf "=== %s === REJECTED: %s\n\n" label msg;
    g

let () =
  Random.self_init ();
  let g = Game.init () in
  show "Initial deal (Player 0 = landlord, gets the kitty)" g;

  (* Player 0 leads with a single (whatever their lowest card is). *)
  let p0_card = List.hd (Player.sorted_hand g.players.(0)) in
  let g = apply_and_show "Player 0 leads a Single" g (Move.Play [ p0_card ]) in

  (* Player 1 passes. *)
  let g = apply_and_show "Player 1 passes" g Move.Pass in

  (* Player 2 passes too -> both opponents passed, trick should reset
     back to Player 0, who leads the next trick. *)
  let g = apply_and_show "Player 2 passes (trick should reset to Player 0)" g Move.Pass in

  assert (g.turn = 0);
  assert (g.table = None);
  Printf.printf "Confirmed: table reset and turn returned to Player 0.\n\n";

  (* Illegal move: try to pass while leading a fresh trick. *)
  let _g_rejected = apply_and_show "Player 0 tries to pass while leading (should fail)" g Move.Pass in

  (* Illegal move: try to play a card not in hand. *)
  let fake_card = Card.make Rank.Three (Some Suit.Spade) in
  let already_played = p0_card in
  let bogus =
    if fake_card = already_played then
      Card.make Rank.Four (Some Suit.Spade)
    else fake_card
  in
  let holds_bogus = List.mem bogus g.players.(0).hand in
  if not holds_bogus then
    ignore (apply_and_show "Player 0 tries to play a card they don't hold (should fail)"
              g (Move.Play [ bogus ]));

  (* Player 0 leads a real pair if they have one, else a single, to show
     the game continuing normally after the reset. *)
  let hand0 = Player.sorted_hand g.players.(0) in
  let rec find_pair = function
    | a :: b :: rest -> if a.Card.rank = b.Card.rank then Some [ a; b ] else find_pair (b :: rest)
    | _ -> None
  in
  let next_play =
    match find_pair hand0 with
    | Some pair -> pair
    | None -> [ List.hd hand0 ]
  in
  let _g = apply_and_show "Player 0 leads again after winning the trick" g (Move.Play next_play) in
  ()

(* ------------------------------------------------------------------ *)
(* Sanity checks for combo classification and comparison               *)
(* ------------------------------------------------------------------ *)



let c r s = Card.make r (Some s)
let sp = Suit.Spade and he = Suit.Heart and cl = Suit.Club and di = Suit.Diamond

let check name cond = Printf.printf "%-45s %s\n" name (if cond then "OK" else "FAIL")

let () =
  (* Straight: 3-4-5-6-7 *)
  let straight = [ c Rank.Three sp; c Rank.Four sp; c Rank.Five sp; c Rank.Six sp; c Rank.Seven sp ] in
  check "3-4-5-6-7 classifies as Straight"
    (match Combo.classify straight with Some (Combo.Straight _) -> true | _ -> false);

  (* Straight can't include a 2 *)
  let bad_straight = [ c Rank.Ten sp; c Rank.Jack sp; c Rank.Queen sp; c Rank.King sp; c Rank.Two sp ] in
  check "10-J-Q-K-2 is NOT a legal straight" (Combo.classify bad_straight = None);

  (* Airplane with single wings: triples 3,4 + two singles *)
  let airplane_single =
    [ c Rank.Three sp; c Rank.Three he; c Rank.Three cl;
      c Rank.Four sp; c Rank.Four he; c Rank.Four cl;
      c Rank.Seven sp; c Rank.Eight sp ]
  in
  check "33344 4+7+8 classifies as Airplane+Singles"
    (match Combo.classify airplane_single with Some (Combo.AirplaneSingle _) -> true | _ -> false);

  (* Bomb beats a straight-shape? no; beats any non-bomb *)
  let bomb = [ c Rank.Nine sp; c Rank.Nine he; c Rank.Nine cl; c Rank.Nine di ] in
  let single = [ c Rank.Two sp ] in
  (match Combo.classify bomb, Combo.classify single with
   | Some b, Some s -> check "Bomb beats a lone Single(2)" (Combo.beats ~incumbent:s b)
   | _ -> check "Bomb beats a lone Single(2)" false);

  (* Rocket beats a bomb *)
  let rocket = [ Card.joker_small; Card.joker_big ] in
  (match Combo.classify rocket, Combo.classify bomb with
   | Some r, Some b -> check "Rocket beats a Bomb" (Combo.beats ~incumbent:b r)
   | _ -> check "Rocket beats a Bomb" false);

  (* Pair of 5s must lose to pair of 9s *)
  let pair5 = [ c Rank.Five sp; c Rank.Five he] in
  let pair9 = [ c Rank.Nine sp; c Rank.Nine he ] in
  (match Combo.classify pair5, Combo.classify pair9 with
   | Some p5, Some p9 -> check "Pair(9) beats Pair(5)" (Combo.beats ~incumbent:p5 p9)
   | _ -> check "Pair(9) beats Pair(5)" false);

  (* Different shapes never beat each other (pair vs triple) *)
  let triple = [ c Rank.Three sp; c Rank.Three he; c Rank.Three cl ] in
  (match Combo.classify pair9, Combo.classify triple with
   | Some p9, Some t -> check "Triple(3) does NOT beat Pair(9) (different shape)"
                           (not (Combo.beats ~incumbent:p9 t))
   | _ -> check "shape mismatch" false)