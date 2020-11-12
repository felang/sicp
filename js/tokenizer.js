let tokenEnum = {
    LBrackets: '(',
    RBrackets: ')',
    Number: '1234567890',
    Lambda: 'lambda',
    Let: 'let',
    Space: ' ',
    Wrap: '\n',
    Quote: '\'',
}

let isNumber = function(s) {
    for (let i = 0; i < s.length; i++) {
        if(!tokenEnum.includes(s[i])) {
            return false
        }
    }

    return true
}

let token = function(exp) {
    let result = []
    let symbol = ''
    for (let i = 0; i < exp.length; i++) {
        const s = exp[i]
        if(s === tokenEnum.LBrackets || s === tokenEnum.RBrackets) {
            if(symbol) {
                result.push(symbol)
                symbol = ''
            }
            result.push(s)
        }else if(s === tokenEnum.Space || s === tokenEnum.Wrap) {
            if(symbol) {
                result.push(symbol)
                symbol = ''
            }
        }else {
            symbol += s
        }
         
    }

    return result
}

class Cons {
    constructor(x, y) {
        this.car = x
        this.cdr = y
    }

    car() {
        return this.car
    }

    cdr() {
        return this.cdr
    }
}
// (let (cons (cons a 1) (cons a '())))
const combine = function(tokens) {
    let result = []
    let stack = []
    let x = []
    let flag = true
    let len = tokens.length
    let t = tokens.slice(1, len-1)
    for(let i = 0; i < t.length; i++) {
        let symbol = t[i]
        if(symbol === tokenEnum.LBrackets) {
            flag = false
            stack.push(symbol)
        }
        if(flag) {
            result.push(symbol)
        }else {
            x.push(symbol)
        }
        if(symbol === tokenEnum.RBrackets) {
            stack.shift()
            if(stack.length === 0) {
                result.push(x)
                x = []
                flag = true
            }
        }
    }
    return result
}

const consToken = function(tokens) {
    if(tokens.length === 0) {
        return null
    }else {
        if(Array.isArray(tokens[0])) {
            return new Cons(consToken(combine(tokens[0])), consToken(tokens.slice(1)))
        }else {
            return new Cons(tokens[0], consToken(tokens.slice(1)))
        }
    }
}

let main = function(exp) {
    // let exp = '(+ 1 (- 333 (+ 1 2)))'
    let tokens = token(exp)
    let t = combine(tokens)
    let c = consToken(t)
    
    return c
}



module.exports = main