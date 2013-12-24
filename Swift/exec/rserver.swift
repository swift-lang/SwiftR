type file;
type RData;

# fixme: extend to enable coaster and non-coaster execution
#        needs to select between EvalRPersistent and simple RunR
#        enable persistent for local mode (not sure how)
#        condense stdout/err to 1 file
#        condense shellscript and rserverscript to one?

app (external e, RData result, file stout, file sterr) runR (file shellscript, file RServerScript, RData rcall, RData exports[])
{
  bash @shellscript @RServerScript @rcall @result "--imports" @exports 
            stdout=@stout stderr=@sterr;
}

app passivate ()
{
  bash "-c" "echo dummy swift job;";
}

process_async(string runDir) {
  external wait[];
  wait = apply(runDir);

  string resultPipeName = @strcat(runDir,"/resultpipe");
  tracef("%k%s done\n", wait, resultPipeName);
  fprintf(resultPipeName, "%kdone\n", wait);
}

(external e[]) apply (string runDir)
{
  RData rcalls[]  <simple_mapper; location=runDir, prefix="cbatch.", suffix=".Rdata", padding=0>;
  RData results[] <simple_mapper; location=runDir, prefix="rbatch.", suffix=".Rdata", padding=0>;
  file  stout[]   <simple_mapper; location=runDir, prefix="stdout.", suffix=".txt", padding=0>;
  file  sterr[]   <simple_mapper; location=runDir, prefix="stderr.", suffix=".txt", padding=0>;

  # Load list of exported files
  string export_list[] = readData(@strcat(runDir, "/exports.txt"));
  RData exports[] <array_mapper; files=export_list>;

  file  runRscript <"EvalRBatchPersistent.sh">;
  file  rsScript   <"SwiftRServer.R">;

  foreach c, i in rcalls {
    (e[i], results[i],stout[i], sterr[i]) = runR(runRscript,rsScript,c, exports);
  }
}

if (@arg("warmup", "false") == "true") {
    passivate();
}

string pipedir = @arg("pipedir");
global string requestPipeName = @strcat(pipedir,"/requestpipe");

iterate serially {
  boolean done;
  // array in case we get multiple requests in one read
  string dirs[];

  trace("top of loop: rserver waiting for input on", requestPipeName);
  dirs = readData(requestPipeName); # Reads direct from this local pipe. Assumes Swift started in right dir.
  done = false;
//  if (dir=="done") { done=true; } else { done=false;}
  foreach dir, i in dirs {
      trace("rserver: got dir", dir);
      if (dir != "") {
          # fork off thread
          process_async(dir);
      }
  }
} until (done);
