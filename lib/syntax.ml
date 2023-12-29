module Option = struct
  let ( let+ ) v f = Option.map f v
  let ( let* ) v f = Option.bind v f
end

module Result = struct
  let ( let+ ) v f = Result.map f v
  let ( let* ) v f = Result.bind v f

  let list_map ~f vs =
    let exception E of string in
    try
      Ok
        (List.map
           (fun v -> match f v with Error msg -> raise (E msg) | Ok v -> v)
           vs )
    with E msg -> Error msg

  let list_iter ~f vs =
    let exception E of string in
    try
      Ok
        (List.iter
           (fun v -> match f v with Error msg -> raise (E msg) | Ok () -> ())
           vs )
    with E msg -> Error msg
end
