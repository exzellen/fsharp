open System.IO
open Microsoft.FSharp.Reflection
open System

type Vehicle(speed: int, distance: int, owner: string) =
    member val Speed =  
        if speed < 0 then 
            failwith("Speed cannot be negative")  
        else speed with get, set 
    member val Distance =  
        if distance < 0 then 
            failwith("Distance cannot be negative") 
        else distance with get, set 
    member val Owner = owner 

type Airplane(parameters: string list) =
    inherit Vehicle(int parameters.[2], int parameters.[3], parameters.[4])
    member val Range = 
        if int parameters.[0] < 0 then
                failwith("Range cannot be negative")
        else int parameters.[0] with get, set
    member val Payload =  
        if int parameters.[1] < 0 then 
            failwith("Payload cannot be negative")  
        else int parameters.[1] with get, set 
    override this.ToString() =
        sprintf "%s %d %d %d %d %s" 
            (this.GetType().Name) this.Range this.Payload base.Speed base.Distance base.Owner

type Train(parameters: string list) =
    inherit Vehicle(int parameters.[1], int parameters.[2], parameters.[3])
    member val Wagons =  
        if int parameters.[0] < 0 then 
            failwith("Number of wagons cannot be negative")  
        else int parameters.[0] with get, set
    override this.ToString() =
        sprintf "%s %d %d %d %s" 
            (this.GetType().Name) this.Wagons base.Speed base.Distance base.Owner

type Truck(parameters: string list) =
    inherit Vehicle(int parameters.[2], int parameters.[3], parameters.[4])
    member val Payload =  
        if int parameters.[0] < 0 then 
            failwith("Payload cannot be negative")  
        else int parameters.[0] with get, set 
    member val Volume =  
        if float parameters.[1] < 0.0 then 
            failwith("Volume cannot be negative") 
        else float parameters.[1] with get, set 
    override this.ToString() =
        sprintf "%s %d %f %d %d %s" 
            (this.GetType().Name) this.Payload this.Volume base.Speed base.Distance base.Owner

type Node(value: Vehicle, next: Node option) =
    member val value = value with get
    member val Next : Node option = next with get, set

type CircularLinkedList() =
    let mutable head = None
    let mutable tail = None
    let mutable count: int = 0

    member this.Add(vehicle: Vehicle) =
        let node = new Node(vehicle, None)
        match tail with
        | None ->
            head <- Some node
            tail <- Some node
            node.Next <- Some node
            count <- count + 1
        | Some t ->
            node.Next <- head
            t.Next <- Some node
            tail <- Some node
            count <- count + 1

    member this.Print() =
        let mutable currentNode = head
        let writer = new System.IO.StreamWriter("output.txt")
        while currentNode <> None && currentNode <> tail do
            printfn "%A" currentNode.Value.value
            writer.WriteLine(currentNode.Value.value.ToString())
            currentNode <- currentNode.Value.Next
        printfn "%A" tail.Value.value
        writer.WriteLine(tail.Value.value.ToString())
        writer.Close()

    member this.rem(parameters: string list) =
        let mutable currentNode : Node option = head
        let mutable count_list : int = count

        while count_list > 0 do
            for cur in currentNode.Value.value.GetType().GetProperties() do
                match cur.Name with
                | value when value = parameters.[0] ->
                    match parameters.[1] with
                    | "==" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue = int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue = float(parameters.[2]) then
                                this.removeNode currentNode
                        | :? string as stringValue -> 
                            if stringValue = parameters.[2] then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | "!=" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue <> int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue <> float(parameters.[2]) then
                                this.removeNode currentNode
                        | :? string as stringValue -> 
                            if stringValue <> parameters.[2] then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | ">=" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue >= int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue >= float(parameters.[2]) then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | "<=" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue <= int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue <= float(parameters.[2]) then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | "<" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue < int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue < float(parameters.[2]) then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | ">" ->
                        let propValue = cur.GetValue(currentNode.Value.value)
                        match propValue with
                        | :? int as intValue -> 
                            if intValue > int(parameters.[2]) then
                                this.removeNode currentNode
                        | :? float as floatValue -> 
                            if floatValue > float(parameters.[2]) then
                                this.removeNode currentNode
                        | _ ->
                            failwith $"Unsupported value type: {propValue.GetType().Name}"
                    | _ ->
                            failwith $"Invalid condition"
                | _ -> ()
            currentNode <- currentNode.Value.Next
            count_list <- count_list - 1

    member this.removeNode (nodeToRemove: Node option) =  
        let mutable currentNode : Node option = head     
        let mutable prevNode : Node option = None  
        while currentNode <> None do  
            if (currentNode = nodeToRemove) then 
                match prevNode with
                | None ->  
                    head <- currentNode.Value.Next  
                    tail.Value.Next <- head
                    count <- count - 1
                    currentNode <- None
                | Some p ->  
                    p.Next <- currentNode.Value.Next  
                    if currentNode = head then 
                        head <- p.Next 
                    if currentNode = tail then  
                        tail <- Some(p)
                    count <- count - 1
                    currentNode <- None
            if currentNode <> None then
                prevNode <- currentNode 
                currentNode <- currentNode.Value.Next
                if currentNode = head then
                    currentNode <- None

let list = new CircularLinkedList()

let readCommands (fileName: string) =    
    use file = new StreamReader(fileName)    
    let commands =  
        seq {   
            for line in file.ReadToEnd().Split([|'\n'|]) do 
                let trimmedLine = line.TrimEnd('\r')
                let parts = trimmedLine.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)    
                match parts.[0] with    
                | "ADD" -> yield ("ADD", parts.[1..] |> Array.toList)  
                | "REM" -> yield ("REM", parts.[1..] |> Array.toList)  
                | "PRINT" -> yield ("PRINT", [])  
                | _ -> printfn "Неверная команда: %s" parts.[0]  
        } 
         |> List.ofSeq    
 
    let savedCommands = commands 
    savedCommands

let add(parameters: string list) =
    match parameters.[0] with
    | "Airplane" -> list.Add(new Airplane(List.tail parameters)) 
    | "Train" -> list.Add(new Train(List.tail parameters))
    | "Truck" -> list.Add(new Truck(List.tail parameters))
    | _ -> printfn $"Invalid product type: {parameters.[0]}"

let executeCommands (commands: (string * string list) list) =
    for cmd in commands do
        match cmd with
        | ("ADD", item) -> add item
        | ("REM", item) -> list.rem(item)
        | ("PRINT", _) -> list.Print()
        | _ -> ()

let fileName = "task-list.txt"
let commands = readCommands fileName
executeCommands commands