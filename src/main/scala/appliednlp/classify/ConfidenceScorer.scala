package appliednlp.classify

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  
  def main(args: Array[String]) {
    val gold = io.Source.fromFile(args(0)).getLines.toList
    val pred = io.Source.fromFile(args(1)).getLines.toList

    val scores = pred.map{ line =>
        val splits = line.split(" ")
        if (splits(1).toDouble > splits(3).toDouble) {
            (splits(1),splits(0))
        } else {
            (splits(3),splits(2))
        }
    }

    val goldLabels = gold.map{ line =>
        val splits = line.split(",")
        splits.takeRight(1).head
    }

    val cor = scores.zip(goldLabels).map(x =>
        if (x._2 == x._1._2) {
            (x._1._1, 1)
        } else (x._1._1, 0)
    ).sorted

    val bucketSize = cor.size/3

    // Bottom Third
    var correct = 0
    var total = 0
    for( ind <- 0 to bucketSize-1) {
        total += 1
        if (cor(ind)._2 == 1) correct += 1
    }
    val bottomPercent = (correct.toDouble/total)*100 

    // Mddle Third
    correct = 0
    total = 0
    for( ind <- bucketSize to (bucketSize*2)-1) {
        total += 1
        if (cor(ind)._2 == 1) correct += 1
    }
    val middlePercent = (correct.toDouble/total)*100 

        // Bottom Third
    correct = 0
    total = 0
    for( ind <- bucketSize*2 to cor.size - 1) {
        total += 1
        if (cor(ind)._2 == 1) correct += 1
    }
    val topPercent = (correct.toDouble/total)*100   


    println("High confidence accuracy: "+topPercent+"%")
    println("Mid confidence accuracy: "+middlePercent+"%")
    println("Low confidence accuracy: "+bottomPercent+"%")

  }  

}
