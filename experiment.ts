/**
 * Finally type level functions. If you see this and you feel the urge
 * to shout my name  in the office, Hold IT. I needed this so I can finally
 * stop using classes for everything bear with me, you don't have to understand
 * it as I myself don't understand all of it(bear in mind everything here
 * I have made sure to understand them). 
 * See https://www.freecodecamp.org/news/typescript-curry-ramda-types-f747e99744ab/
 * for more detail
 */

/**
 * Creates a type-level tuple of the parameters of a function
 * type
 */
export type Parameters<F extends (...args: any[]) => any> =
  F extends ((...args: infer A) => any) ? A : never

/**
 * Gets the first element of a type-level tuple
 */
export type Head<T extends any[]> = T extends [any, ...any[]]
  ? T[0] : never

/**
 * Gets the remaining elements after removing the head of a 
 * type-level tuple.
 */
export type Tail<T extends any[]> =
  ((...t: T) => any) extends
  ((_: any, ...tail: infer U) => any)
  ? U : []

/**
 * Makes sure Tail function doesn't return `[]`
 */
export type HasTail<T extends any[]> = T extends ([] | [any])
  ? false : true

// and now the fun part, recursive types(lick my lips smiley)

/**
 * Get the last element of a type-level tuple
 */
export type Last<T extends any[]> =
  {
    0: Last<Tail<T>>,
    1: Head<T>
  }[HasTail<T> extends true ? 0 : 1]

/**
 * Get the length of a TL tuple
 */
export type Length<T extends any[]> = T['length']

/**
 * Equivalent of cons for TL tuples
 */
export type Prepend<E, T extends any[]> =
  ((head: E, ...args: T) => any) extends ((...args: infer U) => any)
  ? U : T

/**
 * Drop till N items are left in the TL tuple
 * - *N* the number of items needed
 * - *P* the parameters from the function
 * - *C* the counter used to track consumed parameters  
 */
export type Drop<
  N extends Number,
  P extends any[],
  C extends any[] = []
  > =
  {
    0: Drop<N, Tail<P>, Prepend<any, C>>
    1: P
  }[Length<C> extends N ? 1 : 0]


/**
 * Cast(TM), stop Typescript grumbles
 */
export type Cast<A, B> = A extends B ? A : B

export type CastDrop<T extends any[], P extends any[]> = Cast<Drop<Length<T>, P>, any[]>

export type Curry<P extends any[], R> =
  <T extends any[]>(...args: Cast<T, Partial<P>>) =>
    Drop<Length<T>, P> extends [any, ...any[]]
    //@ts-ignore this doesn't work
    ? R : Curry<CastDrop<T, P>, R>

export function curry<P extends any[], R>(fn: (...args: P) => R): any {
  // @ts-ignore becuase type-level madeness and actual implementation
  // in ts are still stupid.
  return function curried(...args: P) {
    if (args.length === fn.length) {
      return fn(...args)
    }
    // @ts-ignore same as above
    return (...args1: P) => curried(...args.concat(args1))
  }
}

export type Curry2<P extends any[], R> =
  <T extends any[]>(...args: T) =>
    Length<T> extends 0 ? Curry2<P, R>
    : Length<T> extends 1 ? Curry2<Drop<1, P>, R>
    : R

export const curry2
  : <P extends any[], R>(fn: (...args: P) => R) => Curry2<P, R>
  = curry