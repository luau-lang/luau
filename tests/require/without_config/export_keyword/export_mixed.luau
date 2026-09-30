-- mixed exports: values, functions, and types together
export local version = "2.0.0"

export type Config = {
    debug: boolean,
    timeout: number,
}

export function createConfig(debug: boolean, timeout: number): Config
    return {
        debug = debug,
        timeout = timeout,
    }
end

export const defaultLabel = "default value"
