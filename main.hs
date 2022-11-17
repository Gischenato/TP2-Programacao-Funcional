{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
main :: IO ()
main = do
    putStr "Digite o caminho do arquivo: "
    fileName <- getLine
    file <- readFile fileName
    let memory = transforma (lines file)
    print "=-=-=-=-="
    print "START"
    print "---------"
    dump memory
    run memory 0 0

run :: [(Int, Int)] -> Int -> Int -> IO ()
run mem pc acc = do
    let check = checkMemory mem
    -- print ( checkMemory mem )
    if not check then error "Overflow or invalid memory position" else do
        let instruct = getMemoryValue mem pc
        print "=-=-=-=-="
        print (instruct, pc, acc)
        print "---------"
        dump mem
        case instruct of
            2  -> lod mem pc acc 
            4  -> sto mem pc acc
            6  -> jmp mem pc acc
            8  -> jmz mem pc acc
            10 -> cpe mem pc acc
            14 -> add mem pc acc
            16 -> sub mem pc acc
            18 -> nop mem pc acc
            20 -> hlt mem pc acc
            _  -> print "Error"


checkMemory :: [(Int, Int)] -> Bool
checkMemory [] = True
checkMemory ((addr, value):xs) = addr >= 0 && addr < 256 && value >= -128 && value < 256 && checkMemory xs

lod :: [(Int, Int)] -> Int -> Int -> IO ()
lod mem pc acc = do
    let addr = getMemoryValue mem (pc + 1)
    let newAcc = getMemoryValue mem addr
    run mem (pc + 2) newAcc

sto :: [(Int, Int)] -> Int -> Int -> IO ()
sto mem pc acc = do
    let addr = getMemoryValue mem (pc + 1)
    let newMem = changeValue mem addr acc
    run newMem (pc + 2) acc

jmp :: [(Int, Int)] -> Int -> Int -> IO ()
jmp mem pc acc = do 
    let newPc = getMemoryValue mem (pc + 1)
    run mem newPc acc

jmz :: [(Int, Int)] -> Int -> Int -> IO ()
jmz mem pc acc = do
    let newPc = getMemoryValue mem (pc + 1)
    case acc of
        0 -> run mem newPc acc
        _ -> run mem (pc + 2) acc

cpe :: [(Int, Int)] -> Int -> Int -> IO ()
cpe mem pc acc = do
    let addr = getMemoryValue mem (pc + 1)
    let value = getMemoryValue mem addr
    if value == acc then run mem (pc + 2) 0 else run mem (pc + 2) 1

add :: [(Int, Int)] -> Int -> Int -> IO ()
add mem pc acc = do 
    let newAcc = acc + getMemoryValue mem (getMemoryValue mem (pc + 1))
    run mem (pc + 2) newAcc

sub :: [(Int, Int)] -> Int -> Int -> IO ()
sub mem pc acc = do
    let newAcc = acc - getMemoryValue mem (getMemoryValue mem (pc + 1))
    run mem (pc + 2) newAcc
    
nop :: [(Int, Int)] -> Int -> Int -> IO ()
nop mem pc acc = do 
    run mem (pc + 2) acc

hlt :: [(Int, Int)] -> Int -> Int -> IO ()
hlt mem pc acc = do 
    print "=-=-=-=-="
    print "TERMINATED"
    print ("mem: ", mem)
    print ("pc : ", pc)
    print ("acc: ", acc)
    print "---------"
    dump mem


changeValue :: [(Int, Int)] -> Int -> Int -> [(Int, Int)]
changeValue [] addr acc = [(addr, acc)]
changeValue ((addr1, actual):xs) addr2 acc = if addr1 == addr2 
                                             then (addr1, acc) : xs 
                                             else (addr1, actual) : changeValue xs addr2 acc

getInt :: IO Int
getInt = readLn

dump :: [(Int, Int)] -> IO ()
dump [] = return ()
dump (mem:tail) = do
    print mem
    dump tail 

toInt :: String -> Int
toInt str = read str:: Int
 
transforma :: [String] -> [(Int, Int)]
transforma [] = []
transforma (str:tail) = (toInt v1, getInstructionValue v2) : transforma tail 
    where v1 = head (words str)
          v2 = last (words str)

getMemoryValue :: [(Int, Int)] -> Int -> Int
getMemoryValue [] _ = -1
getMemoryValue ((addr, value):tail) addr2 | addr == addr2 = value 
                                          | otherwise = getMemoryValue tail addr2

getInstructionValue :: String -> Int
getInstructionValue str | str == "LOD" = 2
             | str == "STO" = 4
             | str == "JMP" = 6
             | str == "JMZ" = 8
             | str == "CPE" = 10
             | str == "ADD" = 14
             | str == "SUB" = 16
             | str == "NOP" = 18
             | str == "HLT" = 20
             | otherwise = toInt str