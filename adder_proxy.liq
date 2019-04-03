(*proxy contract that calls a logic contract to add two numbers and stores the result*)

type storage = {
  adder: address ;
  value: nat } 
contract type ADD  =
sig
  type storage 
  val%entry main : (nat * nat) -> storage -> (operation list * storage)
end

let%init storage = { adder = KT1NaKngVHpvqrMcDbsdQTNvZvTh6JN795QU; value = 0p}

  (*main function: can be called with numbers to be added*)
[%%entry
  let main (_to_forward_add : (tez * (nat*nat))) (storage : storage) =
    let dest = match (Contract.at storage.adder : ADD.instance option) with
      | None -> failwith ("No adder contract found at", storage.adder) 
      | Some c -> c in
    let to_forward = _to_forward_add.(0p)  in
    let add = _to_forward_add.(1p)  in
    let op = Contract.call ~dest ~amount:to_forward ~parameter:add  in
    ([op], storage) ]

  (*updateValue: function to be called by logic contract to update the value in the storage, which will hold the result of the add operation*)
[%%entry
  let updateValue (value : nat) (storage : storage) =
    let storage = storage.value <- value  in 
    ([], storage) ]

  (*setAdder: function to set the address of the logic contract*)
[%%entry
  let setAdder (a : address) (storage : storage) =
    let storage = storage.adder <- a in
    ([], storage) ]
