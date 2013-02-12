namespace FSCL.KernelRunner.MetricTools

open EnergyPatterns.RemoteAmmeter
open System.Diagnostics

exception EnergyMonitoringException of string

type Tools() =
    static member ExcuteFor time beforeStart afterStop f =
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

        let finalIterations = (int) (time * (double)iterations / ((double)timer.ElapsedMilliseconds))
        timer.Reset()
        let startResult = beforeStart() 
        if (startResult <> "OK") then
            raise (EnergyMonitoringException(startResult))
        timer.Start()
        for i in 0 .. finalIterations - 1 do
            f()
        timer.Stop()
        (afterStop(), timer.ElapsedMilliseconds, finalIterations)
        
    static member GetEnergyConsumption ip time f =
        let client = new Client(ip)
        Tools.ExcuteFor time client.start client.stop f
