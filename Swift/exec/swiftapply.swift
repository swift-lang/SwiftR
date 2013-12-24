type file;
type RData;

app (RData result, file stout, file sterr) runR (file shellscript, file initRScript, file RServerScript, RData rcall)
{
  bash @shellscript @initRScript @RServerScript @rcall @result stdout=@stout stderr=@sterr;
#  RunR @rcall @result stdout=@stout stderr=@sterr;
}

app (RData result, file stout, file sterr) RunRv1 (file shellscript, RData rcall)
{
  bash @shellscript @rcall @result stdout=@stout stderr=@sterr;
#  RunR @rcall @result stdout=@stout stderr=@sterr;
}

RData rcalls[]  <simple_mapper; prefix="cbatch.", suffix=".Rdata", padding=0>;
RData results[] <simple_mapper; prefix="rbatch.", suffix=".Rdata", padding=0>;
file  stout[]   <simple_mapper; prefix="stdout.", suffix=".txt", padding=0>;
file  sterr[]   <simple_mapper; prefix="stderr.", suffix=".txt", padding=0>;
#file  runRscript <"RunR.sh">;
file  runRscript <"EvalRBatchPersistent.sh">;
file  initScript <"initialize.R">;
file  rsScript <"SwiftRServer.R">;

foreach c, i in rcalls {
  (results[i],stout[i], sterr[i]) = runR(runRscript,initScript,rsScript,c);
}
