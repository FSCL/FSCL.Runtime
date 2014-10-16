namespace FSCL.Runtime.Scheduling.FeatureExtraction

module GPUMemoryModel =
    type AMD79XXGlobalMemoryModel() =
        static member private channels = 12
        static member private lineSize = 64

        static member private GetChannel(address: int) =
            // Bits 8,9,10 are the virtual pipe
            let pipe = address &&& (7 <<< 8)
            // Bits 9 and 10 select the quadrant
            let quadrant = pipe &&& 6
            // Take bits 11 - 31 and check if % 3
            let channelInQuadrant =
                if (address >>> 11) % 3 = 0 then
                    1
                else
                    // 2 * pipe[0]
                    2 * (quadrant &&& 1)
            // Channel is quadrant * 3 + channelInQuadrant
            quadrant * 3 + channelInQuadrant
            
        static member GetMaxConcurrentAccessesWithStride(stride: int) =
            

