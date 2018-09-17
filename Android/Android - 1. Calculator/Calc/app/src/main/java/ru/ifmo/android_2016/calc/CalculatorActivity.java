package ru.ifmo.android_2016.calc;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

public final class CalculatorActivity extends AppCompatActivity {

    private Button eqvB, clrB, pntB, prtB;
    private Button sumB, subB, mulB, divB;
    private Button [] arrayB;

    private TextView historyT, resultT;

    private StringBuilder [] sides;
    private int operator, side;

    private boolean ready = false;
    private double value;

    @Override
    protected void onCreate (Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_calculator);

        historyT = (TextView) findViewById(R.id.history_preview);
        resultT = (TextView) findViewById(R.id.result_preview);

        eqvB = (Button) findViewById(R.id.eqv);
        eqvB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    value = _evaluate ();
                    ready = true;

                    _repaint ();
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        });

        clrB = (Button) findViewById(R.id.clear);
        clrB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (ready || side == 0) {
                    sides [0] = new StringBuilder ("0");
                    sides [1] = new StringBuilder ();
                } else if (side == 1 && sides [0].length() != 0) {
                    sides [1] = new StringBuilder ();
                } else if (side == 1) {
                    side = 0;
                }

                value = 0;
                operator = -1;

                _repaint ();
            }
        });

        pntB = (Button) findViewById(R.id.point);
        pntB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _addSymbol(11);
            }
        });

        sumB = (Button) findViewById(R.id.sum);
        sumB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _setOperator (1);
            }
        });

        subB = (Button) findViewById(R.id.sub);
        subB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _setOperator (2);
            }
        });

        mulB = (Button) findViewById(R.id.mul);
        mulB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _setOperator (3);
            }
        });

        divB = (Button) findViewById(R.id.div);
        divB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _setOperator (4);
            }
        });

        prtB = (Button) findViewById(R.id.percent);
        prtB.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                _setOperator (5);
            }
        });

        arrayB = new Button[10];
        arrayB[0] = (Button) findViewById(R.id.d0);
        arrayB[1] = (Button) findViewById(R.id.d1);
        arrayB[2] = (Button) findViewById(R.id.d2);
        arrayB[3] = (Button) findViewById(R.id.d3);
        arrayB[4] = (Button) findViewById(R.id.d4);
        arrayB[5] = (Button) findViewById(R.id.d5);
        arrayB[6] = (Button) findViewById(R.id.d6);
        arrayB[7] = (Button) findViewById(R.id.d7);
        arrayB[8] = (Button) findViewById(R.id.d8);
        arrayB[9] = (Button) findViewById(R.id.d9);

        for (int i = 0; i < arrayB.length; i++) {
            final int buttonVal = i;

            arrayB[i].setOnClickListener(new View.OnClickListener() {

                @Override
                public void onClick(View v) {
                    _addSymbol(buttonVal);
                }
            });
        }

        sides = new StringBuilder[2];
        sides [0] = new StringBuilder();
        sides [1] = new StringBuilder();
    }

    private void _addSymbol (int code) {
        if (ready) {
            sides[0] = new StringBuilder();
            sides[1] = new StringBuilder();
            ready = false;

            this.side = 0;
            operator = -1;
        }

        sides [this.side].append (code != 11 ? code : ".");
        _repaint ();
    }

    private void _setOperator (int code) {
        if (ready) {
            sides [0] = new StringBuilder (value + "");
            sides [1] = new StringBuilder ();
            ready = false;

            this.side = 0;
            operator = -1;
        }

        if (side == 0 && sides [0].length () > 0) {
            side = 1;
        } else if (side == 0) {
            sides [0].append ("0");
            side = 1;
        }

        operator = code;
        _repaint ();
    }

    private double _evaluate () {
        double result = 0;

        double left = Double.parseDouble (sides [0].toString ());
        double right = Double.parseDouble (sides [1].toString ());

        if (operator == 1) {
            result = left + right;
        } else if (operator == 2) {
            result = left - right;
        } else if (operator == 3) {
            result = left * right;
        } else if (operator == 4) {
            result = left / right;
        } else if (operator == 5) {
            result = (left / 100) * right;
        }

        return result;
    }

    private void _repaint () {
        String oper = operator == 1 ? "+" : operator == 2 ? "-" : operator == 3 ? "*" : operator == 4 ? "/" : operator == 5 ? "%" : "";

        if (ready) {
            historyT.setText (sides [0].toString () + " " + oper + " " + sides [1].toString ());
            resultT.setText (value + "");
        } else {
            if (side == 1) {
                historyT.setText (sides [0].toString () + " " + oper);
            } else {
                historyT.setText ("");
            }

            resultT.setText (sides [side].toString ());
        }
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState (outState);

        outState.putDouble  ("value", value);
        outState.putString  ("left", sides [0].toString());
        outState.putString  ("right", sides [1].toString ());
        outState.putInt     ("operator", operator);
        outState.putInt     ("side", side);
        outState.putBoolean ("ready", ready);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState (savedInstanceState);

        value        = savedInstanceState.getDouble ("value");
        sides [0]    = new StringBuilder (savedInstanceState.getString ("left"));
        sides [1]    = new StringBuilder (savedInstanceState.getString ("right"));
        operator     = savedInstanceState.getInt ("operator");
        side         = savedInstanceState.getInt ("side");
        ready        = savedInstanceState.getBoolean ("ready");

        _repaint ();
    }

}
