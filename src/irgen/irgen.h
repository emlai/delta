#pragma once

namespace llvm { class Module; }

namespace delta {

class Module;

namespace irgen {
    llvm::Module& compile(const Module& module, bool generateDebugInfo);
}

}
