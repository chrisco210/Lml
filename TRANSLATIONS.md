```
[[const]] = L k . L m . k const m 

[[x]] = L k . L m . k x m 

[[L x . e]] = L k . L m . k (L x . L k' . L m' . [[e]] k' m') m

[[e1 e2]] = 
  L k . L m . 
    [[e1]] 
    (L k' . L m' . 
      [[e2]] (L k'' . L m'' . k' k'' k m'') m' 
    ) 
    m 

[[e1 B e2]] = 
  L k . L m . 
    [[e1]] 
    (L k' . L m' . 
      [[e2]] (L k'' . L m'' . k (k' B k'') m'') m' 
    ) 
    m 

[[U e]] = L k . L m . [[e1]] (L k' . L m' . k (U k') m') m

[[if e1 then e2 else e3]] =
  L k . L m .
    [[e1]] 
    (L k' . L m' . 
      if k' then 
        [[e2]] (L k'' . L m'' . k k'' m'') m'  
      else 
        [[e3]] (L k'' . L m'' . k k'' m'') m'
    )
    m

[[e1; e2]] =
  L k . L m . [[e1]] (L k' . L m' . [[e2]] k m') m

[[while e1 do e2 done]] = L k . L m .
  Z 
  (L f . 
    L m' . 
      [[e1]] 
      (L k'' . L m'' . 
        if k'' then [[e2]] (L k''' . L m''' . f m''') m''
        else k unit m''
      ) m'
  )
  m

[[get]] = L k . L m . k m m

[[set e]] = L k . L m . [[e]] (L k' . L m' . k () k') m

[[ref e]] = [[
  set (get#1 + 1, (get#1 e)::(get#2));
  get#1 - 1
]]

[[deref e]] = [[
  let rec f = fun n lst ->
    if is_nil lst then 
      unit
    else if (n = (hd lst)#1) then
      (hd lst)#2
    else 
      f n (tl lst)
  in
  f e get#2
]]

[[e1 := e2]] = [[
  set (
    get#1,
    let rec f = fun n lst ->
      if is_nil lst then
        []
      else if n = (hd lst)#1 then
        (n, e2)::(f n (tl lst))
      else 
        (hd lst):: (f n (tl lst))
    in
    f e1 get#2
  )
]]
```