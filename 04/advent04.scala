import scala.io.StdIn.readLine

var line = ""
var fullyContainedRanges = 0
var overlappingRanges = 0

def betweenInclusive(start: Int, end: Int, candidate: Int): Boolean =
  start <= candidate && candidate <= end

@main def m() = {
  while({line = readLine(); line != null}){
    val (elf1start:Int, elf1end:Int, elf2start:Int, elf2end:Int) = Tuple.fromArray(
        line  .split(",")
              .flatMap(range => range.split("-"))
              .map(s => s.toInt))
    if(
      (elf1start <= elf2start && elf1end >= elf2end) ||
      (elf2start <= elf1start && elf2end >= elf1end)
    ){
      fullyContainedRanges += 1
    }

    if(
      betweenInclusive(elf1start, elf1end, elf2start) ||
      betweenInclusive(elf1start, elf1end, elf2end)   ||
      betweenInclusive(elf2start, elf2end, elf1start) ||
      betweenInclusive(elf2start, elf2end, elf1end)
    ){
      overlappingRanges += 1
    }
  }

  println(s"Answer 1: ${fullyContainedRanges}") // 513 for my input
  println(s"Answer 2: ${overlappingRanges}") // 878 for my input
}
