package appliednlp.classify

import nak.core._
import nak.data._
import nak.io._
import nak.liblinear._
import nak.maxent._
import java.io._

object FarmAdClassify {
    
    def main(args: Array[String]): Unit = {
    val numFeatures = if (args.size > 0) args(0).toInt else 50

      val ads = io.Source.fromFile("data/farm/farm-ads").getLines.toList.map{ ad =>
        val splits = ad.split(" ")
        val target = if (splits(0).toInt == 1) 1 else 0
        val feats  = splits.drop(1).toList
        (target, feats)
      } 

      val allWords = ads.unzip._2.flatMap(x=>x).groupBy(x=>x).mapValues(_.size).toList.sortBy(_._2).reverse.unzip._1.take(numFeatures)

      val out = ads.map{ ad =>
        feature_vector(ad._1, ad._2, allWords)
      }

      val spl = (out.size * 0.8).toInt
      val outTrain = new java.io.FileWriter("data/farm/farm-ads.train")
      val outTest  = new java.io.FileWriter("data/farm/farm-ads.test")
      for( ind <- 0 to spl) {
          outTrain.write(out(ind)+"\n")
      }
      for( ind <- spl+1 to out.size - 1) {
          outTest.write(out(ind)+"\n")
      }
      outTrain.close
      outTest.close

      val trainingReader = new BufferedReader(new FileReader("data/farm/farm-ads.train"))
      val trainingEvents = 
        new BasicEventStream(new PlainTextByLineDataStream(trainingReader), ",")
    
      val classifier = GIS.trainModel(trainingEvents, 100, 1, 1.0, false)

      val evalReader = new BufferedReader(new FileReader("data/farm/farm-ads.test"))
      val evalEvents = new BasicEventStream(new PlainTextByLineDataStream(evalReader), ",")

      val evalEventsListBuffer = new collection.mutable.ListBuffer[Event]()
      while (evalEvents.hasNext) evalEventsListBuffer.append(evalEvents.next)

      // Get and score the predictions
      val predictions = evalEventsListBuffer.map { event => {
        val scores = classifier.eval(event.getContext)
        (event.getOutcome, scores)
      }}.toList

      val correct = predictions.count { case(trueLabel, scores) => {
        val best = scores.zipWithIndex.maxBy(_._1)._2
        trueLabel == classifier.getOutcome(best)
      }}

      println("Accuracy: " + correct/predictions.length.toDouble*100)

    }

    def feature_vector(outcome:Int, ad: List[String], allWords: List[String]): String = {
        val feats = allWords.map{ word => word+"="+ad.count(_ == word) }
        feats.mkString(",")+","+outcome.toString
    }



}