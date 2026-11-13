type ReactElement<P, T> = { type: T, props: P?, key: string | number | nil, ref: any }
type ReactNode = nil | boolean | number | string | ReactElement<any, any> | { ReactNode? } | { [string]: ReactNode?, UNIQUE_TAG: any? }
type FunctionComponent<P> = (P, any) -> ReactNode
type ClassComponent<P> = { props: P, render: (ClassComponent<P>) -> ReactNode }
type AbstractComponent<P, T> = { render: (P, T) -> ReactNode }
type CreateElement = <P, T>( FunctionComponent<P> | ClassComponent<P> | AbstractComponent<P, T>, P?, ...ReactNode) -> ReactElement<P, T>

local createElement: CreateElement = nil :: any

type CustomTitleBarPortalProps = { displayOrder: number, title: string? }

local function CustomTitleBarPortal(props: CustomTitleBarPortalProps): ReactNode
    return nil
end

local _element = createElement(CustomTitleBarPortal, {
    displayOrder = 151,
})
