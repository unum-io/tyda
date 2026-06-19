package com.choreograph.tyda.table

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Comparable
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.Defaults
import com.choreograph.tyda.Expr
import com.choreograph.tyda.Groupable
import com.choreograph.tyda.Remover
import com.choreograph.tyda.Selector
import com.choreograph.tyda.aggregates.collect
import com.choreograph.tyda.aggregates.max
import com.choreograph.tyda.functions.raiseError
import com.choreograph.tyda.table.ArgsParser.Result

/** Helper class for reading the latest partition of a source.
  *
  * This class is intended to be used inside a args case class and provides a
  * argument that can override resolving the date and force a specific date. The
  * intent of this is to make rerunning a job with an exact date possible.
  * Currently is required to use this class when reading the latest release form
  * a table.
  */
final case class Latest[S <: Source[?, ?], Date](source: S, overrideDate: Option[Date] = None)

object Latest {
  extension [M, Date: Codec: Comparable, PartitionValue: Codec: Selector.To[Date]](
      latest: Latest[Source[M, Partitioner.Hive[PartitionValue]], Date]
  )(using
      decoder: Partitioner.Determinator[PartitionValue, Partitioner.Hive[PartitionValue]],
      creator: Partitioner.Creator[PartitionValue, Partitioner.Hive[PartitionValue]]
  ) {

    /** Determine the latest date partition but no later than `runDate`.
      */
    def latestDate(runDate: Date): Dataset.Single[Option[Date]] =
      latest.overrideDate match {
        case Some(overrideDate) => Dataset.single(Some(overrideDate))
        case None => latest
            .source
            .asPartitionDataset(creator.unfixed)
            .select(_.select[Date])
            .where(_ <= runDate)
            .aggregate(max)
      }
  }
  extension [M: Codec, Date: Codec: Comparable, PartitionValue: Codec: Selector.To[Date]](
      latest: Latest[Source[M, Partitioner.Hive[PartitionValue]], Date]
  )(using
      decoder: Partitioner.Determinator[PartitionValue, Partitioner.Hive[PartitionValue]],
      creator: Partitioner.Creator[PartitionValue, Partitioner.Hive[PartitionValue]]
  ) {

    /** Read the latest date partition but no later than `runDate`.
      */
    def readLatestWithPartitions(runDate: Date): Dataset[(PartitionValue, M)] = {
      val latestDateExpr: Expr[Date] = latest
        .latestDate(runDate)
        .value
        .getOrElse(raiseError[Date](s"No partitions found before or on ${runDate} for ${latest.source}"))
      latest
        .source
        .asDataset(creator.unfixed)
        .withPartitionValues
        .where(v => latestDateExpr == v._1.select[Date])
    }

    def readLatest(runDate: Date): Dataset[M] = readLatestWithPartitions(runDate).select(_._2)
  }

  extension [M: Codec, Date: Codec: Comparable, PartitionValue: Codec: Remover.Of[Date] as r: Selector.To[
    Date
  ]](latest: Latest[Source[M, Partitioner.Hive[PartitionValue]], Date])(using
      decoder: Partitioner.Determinator[PartitionValue, Partitioner.Hive[PartitionValue]],
      creator: Partitioner.Creator[PartitionValue, Partitioner.Hive[PartitionValue]]
  )(using Groupable[r.Out], Codec[r.Out]) {

    /** Read the latest date partition for each partition but no later than
      * `runDate`.
      */
    def readLatestForEachPartitionWithPartitions(runDate: Date): Dataset[(PartitionValue, M)] = {
      val latestDate = latest
        .source
        .asPartitionDataset(creator.unfixed)
        .where(_.select[Date] <= runDate)
        .groupByKey(_.remove[Date])
        .selectValues(_.select[Date])
        .aggregateValue(max)
        .pairs
        .aggregate(collect)
        .value
        .getOrElse(raiseError[Seq[(r.Out, Date)]](s"No partitions found before or on ${runDate} for ${latest
            .source}"))
        .toMap
      latest
        .source
        .asDataset(creator.unfixed)
        .withPartitionValues
        .where { case Expr(p, _) => latestDate.get(p.remove[Date]).contains(p.select[Date]) }
    }

    def readLatestForEachPartition(runDate: Date): Dataset[(Date, M)] =
      readLatestForEachPartitionWithPartitions(runDate).select { case Expr(partition, model) =>
        (partition.select[Date], model)
      }
  }

  private final case class OnlyDate[Date](overrideDate: Option[Date] = None)

  given [M, P <: Partitioner, S <: Source[M, P]: ArgsParser as sourceParser, Date: ArgsParser.Arg]
      : ArgsParser[Latest[S, Date]] with {
    private val dateParser = ArgsParser[OnlyDate[Date]]
    private def dateDefault(default: Option[Latest[S, Date]]): Option[OnlyDate[Date]] =
      default.map(d => OnlyDate(d.overrideDate))
    def parse(
        path: Option[String],
        args: ParsedArgs,
        default: Option[Latest[S, Date]]
    ): (ParsedArgs, Result[Latest[S, Date]]) = {
      val (argsAfterSource, resultSource) = sourceParser.parse(path, args, default.map(_.source))
      val (finalArgs, resultDate) = dateParser.parse(path, argsAfterSource, dateDefault(default))
      (finalArgs, ArgsParser.combine(resultSource, resultDate, (s, d) => Latest(s, d.overrideDate)))
    }
    def serialize(
        path: Option[String],
        t: Latest[S, Date],
        default: Option[Latest[S, Date]],
        templateTag: Option[String]
    ): List[String] =
      val dateSerialized =
        dateParser.serialize(path, OnlyDate(t.overrideDate), dateDefault(default), templateTag)
      sourceParser.serialize(path, t.source, default.map(_.source), templateTag) ++ dateSerialized
    def help(path: Option[String], default: Option[Latest[S, Date]]): ArgsParser.HelpInfo =
      val sourceHelp = sourceParser.help(path, default.map(_.source))
      val dateHelp = dateParser.help(path, dateDefault(default))
      ArgsParser.HelpInfo.Arguments(Seq(sourceHelp, dateHelp))
  }

}
