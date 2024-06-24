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
    OpInfo(Operator oper, uint8_t prec, Assoc assoc, Op opcodeS, Op opcodeU)
        : _oper(oper)
        , _prec(prec)
        , _assoc(assoc)
        , _opcodeS(opcodeS)
        , _opcodeU(opcodeU)
    {
    }

    Operator oper() const { return _oper; }
    Op opcodeS() const { return _opcodeS; }
    Op opcodeU() const { return _opcodeU; }
    uint8_t prec() const { return _prec; }
    Assoc assoc() const { return _assoc; }

  private:
    Operator _oper;
    uint8_t _prec;
    Assoc _assoc;
    Op _opcodeS;
    Op _opcodeU;
};

static std::vector<OpInfo> info {
    { Operator::Equal , 1 , Assoc::Right, Op::NOP  , Op::NOP  }, // OpNode knows if this is an assignment.
    { Operator::AddSto, 1 , Assoc::Right, Op::ADD  , Op::ADD  }, // so all the ...Sto operators (including
    { Operator::SubSto, 1 , Assoc::Right, Op::SUB  , Op::SUB  }, // Equal) just need the binary operation
    { Operator::MulSto, 1 , Assoc::Right, Op::IMUL , Op::UMUL }, // they need to perform. For equal there
    { Operator::DivSto, 1 , Assoc::Right, Op::IDIV , Op::UDIV }, // is no additional operation, so it is
    { Operator::AndSto, 1 , Assoc::Right, Op::AND  , Op::AND  }, //
    { Operator::OrSto , 1 , Assoc::Right, Op::OR   , Op::OR   }, //
    { Operator::XorSto, 1 , Assoc::Right, Op::XOR  , Op::XOR  }, //
    { Operator::LOr   , 2 , Assoc::Left , Op::NOP  , Op::NOP  }, // Logical AND, OR and NOT don't have opcodes.
    { Operator::LAnd  , 3 , Assoc::Left , Op::NOP  , Op::NOP  }, // They are short-circuited at compile time.
    { Operator::Or    , 4 , Assoc::Left , Op::OR   , Op::OR   },
    { Operator::Xor   , 5 , Assoc::Left , Op::XOR  , Op::XOR  },
    { Operator::And   , 6 , Assoc::Left , Op::AND  , Op::AND  },
    { Operator::EQ    , 7 , Assoc::Left , Op::EQ   , Op::EQ   }, // FIXME: How do the branch opcodes relate to these operators?
    { Operator::NE    , 7 , Assoc::Left , Op::NE   , Op::NE   },
    { Operator::LT    , 8 , Assoc::Left , Op::LT   , Op::LO   },
    { Operator::GT    , 8 , Assoc::Left , Op::GT   , Op::HI   },
    { Operator::GE    , 8 , Assoc::Left , Op::GE   , Op::HS   },
    { Operator::LE    , 8 , Assoc::Left , Op::LE   , Op::LS   },
    { Operator::Plus  , 10, Assoc::Left , Op::ADD  , Op::ADD  },
    { Operator::Minus , 10, Assoc::Left , Op::SUB  , Op::SUB  },
    { Operator::Mul   , 11, Assoc::Left , Op::IMUL , Op::UMUL },
    { Operator::Div   , 11, Assoc::Left , Op::IDIV , Op::UDIV },
};

inline bool findOpInfo(Operator oper, OpInfo& opInfo)
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
