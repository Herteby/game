module Css exposing (..)


css =
    """
html {
    height:100%;
}
body {
    background:#222;
    color:white;
    margin:0;
    overflow:hidden;
    font-family:sans-serif;
    height:100%;
    display:flex;
    flex-direction:column;
    font-size:16px;
}

button {
    background:#06f;
    color:#fff;
    border:none;
    border-radius:5px;
    cursor:pointer;
    padding:5px 15px;
}

.main {
    display:flex;
    flex-direction:column;
    align-items:center;
    justify-content:center;
    flex-grow:1;
}

.startPage {
    display:flex;
    flex-direction:column;
}

.startPage button {
    font-size:32px;
    margin-bottom:20px;
}

.form {
    display:flex;
    flex-direction:column;
    background:#333;
    border-radius:10px;
    padding:20px;
    width:200px;
    margin:auto;
}

.form label {
    display:flex;
    flex-direction:column;
    margin-bottom:15px;
}




.characterPicker {

}
.character {
    width:78px;
    height:120px;
    border:none;
    background-color:transparent;
    outline:none;
    background-size: 234px 432px;
    background-position: center top;
    image-rendering:pixelated;
    margin:10px;
    border-radius:10px;
    cursor:pointer;
}
.character:hover {
    animation: walk 0.5s linear infinite;
}

.character.selected {
    animation: walk 0.5s linear infinite;
    background-color:rgba(0,0,0,0.2);
}

@keyframes walk {
    0% {background-position: center top;}
    25% {background-position: center top;}
    25.0001% {background-position: left top;}
    50% {background-position: left top;}
    50.0001% {background-position: center top;}
    75% {background-position: center top;}
    75.0001% {background-position: right top;}
    100% {background-position: right top;}
}

"""
