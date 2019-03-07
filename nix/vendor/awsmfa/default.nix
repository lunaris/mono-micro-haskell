{ lib
, python
}:

python.pkgs.buildPythonApplication rec {
  pname = "awsmfa";
  version = "0.2.9";

  src = python.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "1a7kw2gmwndalgk4adkskv6jy9dl4ivrjb1vi8y0hpq6pkharygr";
  };

  # No tests included
  doCheck = false;

  propagatedBuildInputs = with python.pkgs; [
    botocore
    boto3
    pytz
    six
  ];

  meta = with lib; {
    homepage = https://github.com/dcoker/awsmfa/;
    description = "Manage temporary MFA AWS credentials";
    license = licenses.asl20;
  };
}

