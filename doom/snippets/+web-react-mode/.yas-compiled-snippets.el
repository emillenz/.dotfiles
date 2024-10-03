;;; Compiled snippets and support files for `+web-react-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets '+web-react-mode
                     '(("useState" "const [${1:state}, set${1:$(s-upper-camel-case yas-text)}] = useState(${2:initialState});\n$0" "useState" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/useState" nil nil)
                       ("useEffect" "useEffect(() => {\n  $0\n});\n" "useEffect" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/useEffect" nil nil)
                       ("shouldComponentUpdate" "shouldComponentUpdate(nextProps, nextState) {\n  $0\n}\n" "shouldComponentUpdate" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/shouldComponentUpdate" nil nil)
                       ("rren" "ReactDOM.render(<${1:ComponentName} />, document.${2:body});" "ReactDOM.render(component, node)" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/render" nil "rren")
                       ("reducer" "const initialState = {\n  $1\n};\n\nexport default (state = initialState, action) => {\n  switch (action.type) {\n    $0\n\n    default:\n      return state;\n  }\n};\n" "reducer" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/reducer" nil nil)
                       ("mergeProps" "const mergeProps = (stateProps, dispatchProps, ownProps) => ({\n  ...stateProps,\n  ...dispatchProps,\n  ...ownProps,\n  $0\n});\n" "mergeProps" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/mergeProps" nil nil)
                       ("getSnapshotBeforeUpdate" "static getSnapshotBeforeUpdate(prevProps, prevState) {\n  $0\n  return null;\n}\n" "getSnapshotBeforeUpdate" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/getSnapshotBeforeUpdate" nil nil)
                       ("getDerivedStateFromProps" "static getDerivedStateFromProps(nextProps, prevState) {\n  $0\n  return null;\n}\n" "getDerivedStateFromProps" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/getDerivedStateFromProps" nil nil)
                       ("container" "import { connect } from 'react-redux';\n\nconst mapStateToProps = (state, ownProps) => ({\n  $0\n});\n\nconst mapDispatchToProps = (dispatch, ownProps) => ({\n\n});\n\nconst ${1:`(f-base buffer-file-name)`} = connect(\n  mapStateToProps,\n  mapDispatchToProps\n)(${2:Component});\n\nexport default $1;\n" "Redux container" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/container" nil nil)
                       ("componentWillUnmount" "componentWillUnmount() {\n  $0\n}\n" "componentWillUnmount" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/componentWillUnmount" nil nil)
                       ("componentDidUpdate" "componentDidUpdate() {\n  $0\n}" "componentDidUpdate" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/componentDidUpdate" nil nil)
                       ("componentDidMount" "componentDidMount() {\n  $0\n}" "componentDidMount" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/componentDidMount" nil nil)
                       ("component-class" "import { Component } from 'react';\n\nclass ${1:`(f-base buffer-file-name)`} extends Component {\n  render() {\n    return (\n      $0\n    );\n  }\n}\n\nexport default $1;" "React component class" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/component-class" nil nil)
                       ("component" "import { Component } from 'react';\n\nconst ${1:`(f-base buffer-file-name)`} = (props) => (\n  $0\n);\n\nexport default $1;" "React component" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/component" nil nil)
                       ("action" "const ${1:$(upcase (s-snake-case yas-text))} = '${1:$(upcase (s-snake-case yas-text))}';\n\nexport const ${1:actionName} = (${2:args}) => ({\n  type: '${1:$(upcase (s-snake-case yas-text))}',\n  payload: {\n    $0\n  },\n});\n" "action" nil nil nil "/home/lenz/.config/doom/snippets/+web-react-mode/action" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:46 2024
