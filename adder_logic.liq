(* logic contract to add 2 numbers and return the result *)
contract type Proxy  =
sig
  type storage 
  val%entry main : (tez* (nat * nat)) -> storage -> (operation list * storage)
  val%entry updateValue : nat -> storage -> (operation list * storage)
  val%entry setAdder : address -> storage -> (operation list * storage)
end

type storage = {
  retval : nat;
  proxy : address }

(* instantiate the storage of the logic contract with the address of the proxy as a parameter *)
let%init storage (proxyaddress : address) = { retval = 0p; proxy = proxyaddress }
     
                                                                                   
(* main: function that does the adding logic, adding two natural numbers and storing the result in storage, 
after that the result is sent back to the proxy *)
[%%entry
  let main (to_add : (nat*nat)) (storage : storage) =
    let storage = storage.retval <- to_add.(0p) + to_add.(1p)  in 
    updateProxy(storage) ] 

(* updateProxy: function that calls the updateValue function of the proxy, to send the result of the logic back to the proxy *)
let updateProxy (storage) = 
  let dest = match (Contract.at storage.proxy : Proxy.instance option) with
    | None -> failwith ("No proxy found at address ", storage.proxy)
    | Some c -> c in
  let result = storage.retval in
  let op = Contract.call ~dest ~amount:0tz ~entry:updateValue ~parameter:result in
  ([op], storage)