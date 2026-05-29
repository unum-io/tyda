package com.choreograph.tyda.job
import com.choreograph.tyda.table.ArgsParser
import com.choreograph.tyda.table.ArgsParser.Arg
import com.choreograph.tyda.table.SourceSinkTraversal

enum CheckpointArg {
  case Test(verify: Seq[Any] => Any)
  case Path(basePath: String)
}

object CheckpointArg {
  given ArgsParser[CheckpointArg] = {
    given Arg[Seq[Any] => Any] = Arg.javaSerializedBase64[Seq[Any] => Any]
    ArgsParser.sumUntagged
  }
  given SourceSinkTraversal[CheckpointArg] = SourceSinkTraversal.empty

  def test: CheckpointArg = CheckpointArg.Test(_ => ())
}
