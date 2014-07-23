<jobStatus interval="300">
  <ifRunning>
    <elapsedSeconds/> seconds elapsed

    <div class="progress">
      <div class="progress-bar progress-bar-striped active"  role="progressbar"
           aria-valuenow="${amountCompleted}" aria-valuemin="0" aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
        <percentCompleted/>%
      </div>
    </div>

  </ifRunning>
  <ifFinished>
    <div class="progress">
      <div class="progress-bar progress-bar-striped"  role="progressbar"
           aria-valuenow="${amountCompleted}" aria-valuemin="0" aria-valuemax="${amountTotal}" style="width: ${percentCompleted}%">
        <percentCompleted/>%
      </div>
    </div>
  </ifFinished>
</jobStatus>
