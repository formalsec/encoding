module Option : sig
  val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
  val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
end

module Result : sig
  val ( let+ ) : ('a, 'e) Result.t -> ('a -> 'b) -> ('b, 'e) Result.t

  val ( let* ) :
    ('a, 'e) Result.t -> ('a -> ('b, 'e) Result.t) -> ('b, 'e) Result.t

  val list_map :
    f:('a -> ('b, string) Result.t) -> 'a list -> ('b list, string) Result.t

  val list_iter :
    f:('a -> (unit, string) Result.t) -> 'a list -> (unit, string) Result.t
end
