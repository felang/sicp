const tokenizer = require('./tokenizer')

const eval = function(exp, env) {
    if(isSelfEvaluating(exp)) {
        return exp
    }
    if(isVariable(exp)) {
        return lookup(exp, env)
    }
    if(isAssignment(exp)) {
        return evalAssignment(exp, env)
    }
    if(isDefinition(exp)) {
        return evalDefinition(exp, env)
    }
    if(isIf(exp)) {
        return evalIf(exp, env)
    }
    if(isLambda(exp)) {
        return makeProcedure(lambdaParams(exp), labmdaBody(exp), env)
    }
    if(isApplication(exp)) {
        return apply(eval(operator(exp), env), listOfValues(operands(exp), env))
    }

    return "error"
}

let isNumber = function(s) {
    let n = Number(s)
    if(!isNaN(n)) {
        return true
    }
    return false
}

let isSelfEvaluating = (s) => { return isNumber(s)}

const main = function() {

}

main()