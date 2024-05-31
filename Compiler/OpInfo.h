/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

namespace lucid {

enum class Assoc { None, Left, Right };
    
class OpInfo {
  public:
    OpInfo() { }
    
    // assign says this is an assignmentOperator, opAssign says it also has a binary op
    OpInfo(Operator oper, uint8_t prec, Assoc assoc)
        : _oper(oper)
        , _prec(prec)
        , _assoc(assoc)
    {
    }
    
    Operator oper() const { return _oper; }
    uint8_t prec() const { return _prec; }
    Assoc assoc() const { return _assoc; }

  private:
    Operator _oper;
    uint8_t _prec;
    Assoc _assoc;
};

static std::vector<OpInfo> info {
    { Operator::Equal , 1 , Assoc::Right },
    { Operator::AddSto, 1 , Assoc::Right },
    { Operator::SubSto, 1 , Assoc::Right },
    { Operator::MulSto, 1 , Assoc::Right },
    { Operator::DivSto, 1 , Assoc::Right },
    { Operator::ModSto, 1 , Assoc::Right },
    { Operator::AndSto, 1 , Assoc::Right },
    { Operator::OrSto , 1 , Assoc::Right },
    { Operator::XorSto, 1 , Assoc::Right },
    { Operator::LOr   , 2 , Assoc::Left  },
    { Operator::LAnd  , 3 , Assoc::Left  },
    { Operator::Or    , 4 , Assoc::Left  },
    { Operator::Xor   , 5 , Assoc::Left  },
    { Operator::And   , 6 , Assoc::Left  },
    { Operator::EQ    , 7 , Assoc::Left  },
    { Operator::NE    , 7 , Assoc::Left  },
    { Operator::LT    , 8 , Assoc::Left  },
    { Operator::GT    , 8 , Assoc::Left  },
    { Operator::GE    , 8 , Assoc::Left  },
    { Operator::LE    , 8 , Assoc::Left  },
    { Operator::Plus  , 10, Assoc::Left  },
    { Operator::Minus , 10, Assoc::Left  },
    { Operator::Mul   , 11, Assoc::Left  },
    { Operator::Div   , 11, Assoc::Left  },
    { Operator::Mod   , 11, Assoc::Left  },
};

bool findOpInfo(Operator oper, OpInfo& opInfo)
{
    auto it = find_if(info.begin(), info.end(),
                    [oper](const OpInfo opInfo) { return opInfo.oper() == oper; });
    if (it != info.end()) {
        opInfo = *it;
        return true;
    }
    return false;
}
    

}
