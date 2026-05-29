---
title: TydaJob
---

# TydaJob

`TydaJob` is a framework for writing data processing jobs with automatic CLI argument parsing.
Define your job arguments as a case class, and the framework derives command-line flags automatically.

## Example

This example shows a job that joins users with their events, counts events per user, and writes the results to a stats table.
All tables are partitioned by date.

### Date Partitioning

Tables are commonly partitioned by date to enable efficient querying and incremental processing.
Define a `DatePartition` case class and an extension method to read data for a specific date:

```scala mdoc
import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Dataset
import com.choreograph.tyda.table.Partitioner
import com.choreograph.tyda.table.Source

final case class DatePartition(date: Date)

object DatePartition {
  extension [M: Codec](source: Source[M, Partitioner.Hive[DatePartition]]) {
    def read(date: Date): Dataset[M] = {
      val partitioner = Partitioner.Hive.fromValue(DatePartition(date))
      source.asDataset(partitioner).values
    }
  }
}
```

### Models and Tables

Define table objects to declare the schema (model), partitioning strategy, and storage prefix in one place.
This makes it explicit which jobs read from or write to the same location, and avoids repeating the model and partitioner types across job definitions:

```scala mdoc
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.Codec
import com.choreograph.tyda.table.PathTable
import com.choreograph.tyda.table.Partitioner

opaque type UserId = Long
object UserId {
    given Arbitrary[UserId] = Arbitrary[Long]
    given Codec[UserId] = Codec[Long]
}

final case class User(id: UserId, name: String)
final case class Event(userId: UserId, eventType: String)
final case class UserEventStats(userId: UserId, eventType: String, eventCount: Long)

// Table objects tie together model, partitioner, and path prefix.
// Jobs reference these to declare their inputs/outputs, making
// data dependencies between jobs explicit and easy to trace.
object UserTable extends PathTable[User, Partitioner.Hive[DatePartition]] {
  val prefix = "users"
}

object EventTable extends PathTable[Event, Partitioner.Hive[DatePartition]] {
  val prefix = "events"
}

object UserEventStatsTable extends PathTable[UserEventStats, Partitioner.Hive[DatePartition]] {
  val prefix = "user-event-stats"
}
```

### Job Definition

Then define the job:


<!---
Sadly we have to skip mdoc here since when this gets used inside a class (that's how mdoc works) it causes a compiler crash.
See: https://github.com/scala/scala3/issues/22704
-->
```scala
import com.choreograph.tyda.Date
import com.choreograph.tyda.Expr
import com.choreograph.tyda.aggregates.count
import com.choreograph.tyda.job.TydaJob
import com.choreograph.tyda.job.TydaJobContext

final case class UserEventStatsJob(
    date: Date,
    users: UserTable.Source,
    events: EventTable.Source,
    output: UserEventStatsTable.Sink
)

object UserEventStatsJob extends TydaJob[UserEventStatsJob] {
  override def run(args: UserEventStatsJob)(using TydaJobContext): Unit = {
    val users = args.users.read(args.date)
    val events = args.events.read(args.date)

    users
      .join(events, (u, e) => u.id == e.userId)
      .groupBy { case Expr(user, event) => (userId = user.id, eventType = event.eventType) }
      .aggregate(_ => (eventCount = count())
      .as[UserEventStats]
      .write(args.output, DatePartition(args.date))
  }
}
```

Run the job:

```bash
spark-submit --class com.example.UserEventStatsJob app.jar \
  --runner spark \
  --date 2024-01-15 \
  --users-base-path gs://bucket/data/ \
  --events-base-path gs://bucket/data/ \
  --output-base-path gs://bucket/output/
```

## Testing

Use `testJob` to test jobs with in-memory data.
Replace `Source` and `Sink` with their test variants:

```scala
import com.choreograph.tyda.Arbitrary
import com.choreograph.tyda.job.test.testJob
import com.choreograph.tyda.table.Sink
import com.choreograph.tyda.table.Source
import org.scalatest.funsuite.AnyFunSuite

class UserEventStatsJobSpec extends AnyFunSuite {
  val date = Arbitrary[Date]()
  val user1 = Arbitrary[UserId]()
  val user2 = Arbitrary[UserId].filter(_ != user1)()

  test("counts events per user and event type") {
    testJob(UserEventStatsJob(
      date = date,
      users = Source.Test(Seq(
          User(user1, "Alice"),
          User(user2, "Bob")
        )
      ),
      events = Source.Test(Seq(
          Event(user1, "click"),
          Event(user1, "click"),
          Event(user1, "view"),
          Event(user1, "click")
        )
      ),
      output = Sink.Test { (stats: Seq[UserEventStats]) =>
        assert(stats.size == 3)
        assert(stats.contains(UserEventStats(user1, "click", 2L)))
        assert(stats.contains(UserEventStats(user1, "view", 1L)))
        assert(stats.contains(UserEventStats(user2, "click", 1L)))
      }
    ))
  }
}
```

`Source.Test` provides test data, and `Sink.Test` takes a verification function that receives the output.
For partitioned tables, provide data as `partition -> Seq(...)` pairs.
