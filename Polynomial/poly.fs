module Polynomial

open System

// Type structure
    
    type Polynomial = Polynomial of list<int>

    let PolyPrint (Polynomial poly) = 
        poly
        |> List.rev
        |> List.mapi (fun i x -> x,i)
        |> List.rev
        |> List.map (fun (x,i) -> 
        match i with 
        | 0 -> printf "%A " x
        | 1 -> printf "%Ax " x  
        |_ -> printf "%Ax^%A " x i )
    //Works

    let PolyAdd (Polynomial polyA) (Polynomial polyB) = 
    //Adds two polynomials together
        let lengthA = polyA.Length
        let lengthB = polyB.Length
        match (lengthA,lengthB) with 
        | a,b when a = b -> 
            polyA 
            |> List.map2 (fun (x:int) (y:int) -> x + y) polyB
            |> Polynomial
        | a,b when a > b ->
            List.init (a-b) (fun x -> 0) @ polyB 
            |> List.map2 (fun (x:int) (y:int) -> x + y) polyA
            |> Polynomial
        | a,b when a < b ->
            List.init (b-a) (fun x-> 0) @ polyA
            |> List.map2 (fun x y ->  x + y) polyB
            |> Polynomial
        | d -> 
            failwithf "Should never happen, d = %A" d
    //WORKS

    let PolySubtraction (Polynomial polyA) (Polynomial polyB) = 
        //Returns the sum polyA - polyB
        let diff = polyA.Length - polyB.Length
        match diff with
        | 0 ->
            List.map2 (fun x y -> x - y) polyA polyB
            |> Polynomial
        | d when d < 0 ->
            List.init (-d) (fun x -> 0) @ polyA
            |> List.map2 (fun y x -> x - y) polyB
            |> Polynomial
        | d when d > 0 ->
            List.init (d) (fun x -> 0) @ polyB
            |> List.map2 (fun x y -> x - y) polyA
            |> Polynomial
        | d -> 
            failwithf "Should never happen, d = %A" d
    //Works

    let degreeofpolynomial (Polynomial poly) = 
        //Returns the degree of the polynomial as an option type
        //NB the zero polynomial has degree 0 as with all constant polynomials
        poly 
        |> List.tryFindIndex (fun x -> x <> 0)
        |> function
            | Some index -> 
                Some ((List.length poly) - index - 1)
            | None ->
                match List.length poly with 
                | 0 -> None
                | _ -> Some 0
    //Works

    let RemoveLeadingZero (Polynomial poly) =
        // given a polynomial, returns a new polynomial, with the leading zeros removed. 
        //if it's an zero polynomial, it returns an empty polynomial
        poly 
        |> List.tryFindIndex (fun x -> x <> 0)
        |> function
            | Some index -> 
                poly.[index..]
                |> Polynomial
            | None ->
                [] |> Polynomial
    //WORKS

    let rec PolyMultiply (Polynomial polyA) (Polynomial polyB) = 
        match polyB.Length with
        | x when x > 0 ->
            //dsfdsf
            let temp = polyA 
                        |> List.map (fun x -> x * polyB.Head)
                        |> fun x -> List.append x (List.init (polyB.Length-1) (fun x->0))
                        |> Polynomial
            polyB.[1..] 
                |> Polynomial
                |> PolyMultiply (Polynomial polyA)
                |> PolyAdd temp
            //sdfsdfdsf
        | 0 ->
            Polynomial []
        | x -> failwithf "Should never happen. PolyB.Length = %A" x
        // SEEMS TO WORK

    type PolDivisionResult = {qoutient:Polynomial; reminder:Polynomial} 

    let PolyDivision (polyA: Polynomial) (polyB: Polynomial ) = 
        //Divides polyA on polyB and returns the qoutient and reminder as a record type
        let rec DividePoly (polyA: Polynomial) (polyB: Polynomial) (ansarr: Polynomial) :Polynomial*Polynomial = 
            //given to polynomials A,B returns the qoutient of A/B and the reminder
            //expects order(A)>order(B)
                let degreeA = polyA
                              |> RemoveLeadingZero  //This is done to get None, if we have a zero polynomial
                              |> degreeofpolynomial
                let degreeB = polyB
                              |> RemoveLeadingZero  //This is done to get None, if we have a zero polynomial
                              |> degreeofpolynomial
                              |> function 
                                 | Some value -> 
                                    value
                                 |None -> 
                                    failwithf "Trying to divide by zero or empty, polyB : %A" polyB

                let (Polynomial listA) = polyA 
                let (Polynomial listB) = polyB
                match degreeA with
                | None ->
                    (ansarr, polyA)
                | Some degA ->
                    match degA with 
                    | A when A = degreeB ->
                        let tempans = listA.Head / listB.Head
                                      |> fun x -> Polynomial[x]
                        let newA = 
                            tempans
                            |> PolyMultiply polyB
                            |> PolySubtraction polyA
                        tempans |> PolyAdd ansarr
                                |> DividePoly (RemoveLeadingZero newA) polyB 
                    | A when A > degreeB -> 
                        let temp  = listA.Head / listB.Head
                        let tempans = 
                            A - degreeB 
                            |> (fun x ->  [for i in 1..x -> 0])
                            |> List.append [temp]
                            |> Polynomial
                        let newA = 
                            tempans 
                            |> PolyMultiply polyB
                            |> PolySubtraction polyA
                        tempans |> PolyAdd ansarr
                                |> DividePoly (RemoveLeadingZero newA) polyB 
                    | _ ->
                        (ansarr, polyA)
        
        let degA = polyA 
                       |> RemoveLeadingZero 
                       |> degreeofpolynomial
        let degB = polyB 
                       |> RemoveLeadingZero
                       |> degreeofpolynomial
        match degA,degB with 
        | None,_ -> 
            failwithf "Trying to divide zero or empty polynomial, polyA = %A" polyA
        |_,None ->
            failwithf "Trying to divide zero or empty polynomial, polyA = %A" polyB
        | Some a,Some b ->
                match a,b with 
                | a,b when a < b ->
                    {qoutient = Polynomial [0] ; reminder= polyB}
                | _,_ ->
                    let q,r = DividePoly polyA polyB (Polynomial [])
                    {qoutient = q ; reminder= r }
    //Seems to work



