package net.edmacdonald.adventofcode

fun main() {
    val move = mapOf(
        "A" to "R",
        "B" to "P",
        "C" to "S",
        "X" to "R",
        "Y" to "P",
        "Z" to "S",
    )

    val score = mapOf(
        "R" to 1,
        "P" to 2,
        "S" to 3
    )

    fun beats(myMove: String, theirMove:String): Boolean =
        (myMove == "R" && theirMove == "S") ||
        (myMove == "P" && theirMove == "R") ||
        (myMove == "S" && theirMove == "P")

    fun ties(myMove: String, theirMove:String): Boolean =
        myMove == theirMove

    fun score(theirMove:String, myMove:String):Int =
        score[myMove]!! +
        if(beats(myMove, theirMove)) { 6 }
        else if (ties(myMove, theirMove)){ 3 }
        else{ 0 }

    fun toLose(theirMove:String): String = mapOf("R" to "S", "P" to "R", "S" to "P")[theirMove]!!
    fun toDraw(theirMove:String): String = theirMove
    fun toWin(theirMove:String): String = mapOf("R" to "P", "P" to "S", "S" to "R")[theirMove]!!

    val desiredOutcome = mapOf(
        "X" to ::toLose,
        "Y" to ::toDraw,
        "Z" to ::toWin
    )


    var line = readLine()
    var totalScore1 = 0
    var totalScore2 = 0
    while(line != null)   {
        val(theirMove, column2) = line.trim().split("""\s+""".toRegex(), 2)

        val s1 = score(move[theirMove]!!, move[column2]!!)
        totalScore1 += s1

        val s2 = score(move[theirMove]!!, desiredOutcome[column2]!!(move[theirMove]!!))
       totalScore2 += s2

        line = readLine()
    }

    println("Total Score (Part 1): ${totalScore1}") //17189 for my input.txt
    println("Total Score (Part 2): ${totalScore2}") //13490
}
