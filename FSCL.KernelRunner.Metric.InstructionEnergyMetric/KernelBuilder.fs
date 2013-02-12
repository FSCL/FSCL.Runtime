namespace FSCL.KernelRunner.Metric.InstructionEnergyMetric

type internal KernelBuilder() =
    static member BuildUnrolledKernel(numOps) =
        let mutable kernel = "kernel void run(global float* input, global float* output) {\n                                 
                                 volatile float e1 = input[0];\n
                                 volatile float e2 = 10;\n"
        for i = 0 to numOps - 1 do
            if (i % 2 = 0) then
                kernel <- kernel + "e1 += e2;\n";
            else
                kernel <- kernel + "e2 += e1;\n";
        
        if ((numOps - 1) % 2 = 0) then
            kernel <- kernel + "output[0] = e1;\n}\n";
        else
            kernel <- kernel + "output[0] = e2;\n}\n";

        kernel

    static member BuildLoopKernel() =
        let mutable kernel = "kernel void run(global float* input, global float* output, int n) {\n                                 
                                 volatile float e1 = input[0];\n
                                 volatile float e2 = 10;\n
                                 for(int i = 0; i <= n; i++) {\n"
        kernel <- kernel + "e1 += e2;\n";
        kernel <- kernel + "e2 += e1;\n";
        
        kernel <- kernel + "}\n"
        kernel <- kernel + "output[0] = e2;\n}\n";

        kernel

        

