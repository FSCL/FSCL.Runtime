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
        let client = new Client(ip, "1")
        Tools.ExcuteFor time client.start client.stop f
        
    static member InterpolateResults(findValue: double, prof: 'T list, getData: 'T -> double * double * double) =
        let mutable found = false
        let mutable findIndex = 0
        let mutable prevInstrCount = 0.0
        let mutable nextInstrCount = 0.0
        let mutable prevEnergy = 0.0
        let mutable nextEnergy = 0.0
        while(not found && findIndex < prof.Length) do 
            let value, time, iterations = getData(prof.[findIndex])
            if(value > findValue) then
                found <- true
                nextEnergy <- (time / 1000.0 / iterations) * value
                nextInstrCount <- (double)value
                if(findIndex > 0) then
                    let value, time, iterations = getData(prof.[findIndex - 1])
                    prevEnergy <- (time / 1000.0 / iterations) * value
                    prevInstrCount <- (double)value
            findIndex <- findIndex + 1
            
        let estimateEnergy = prevEnergy + ((nextEnergy - prevEnergy) * ((double)findValue - prevInstrCount) / (nextInstrCount - prevInstrCount))
        estimateEnergy