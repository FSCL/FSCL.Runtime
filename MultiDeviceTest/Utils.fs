 module Utils
    open System.Diagnostics

    let ExcuteFor time f =
        let timer = Stopwatch()
        let mutable iterations = 1
        while (timer.ElapsedMilliseconds < 100L) do
            timer.Reset()
            timer.Start()
            for i in 0 .. iterations do
                f()
            timer.Stop()

            if (timer.ElapsedMilliseconds < 100L) then
                iterations <- iterations * 10

        let mutable finalIterations = (int) (time * (double)iterations / ((double)timer.ElapsedMilliseconds))
        if finalIterations = 0 then
            finalIterations <- 1

        timer.Reset()
        timer.Start()
        for i in 0 .. finalIterations - 1 do
            f()
        timer.Stop()
        (((float)timer.ElapsedMilliseconds) / (float)finalIterations, finalIterations)